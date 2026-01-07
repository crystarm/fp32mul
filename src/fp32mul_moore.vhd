-- fp32mul_moore.vhd
-- IEEE-754 binary32 multiplier
-- implemented as a Moore FSM + registered datapath.

-- Notes:
--   * No reset port. Internal signals have default initial values (GHDL-friendly).
--   * Handshake: pulse start=1 for 1 cycle in IDLE, wait done=1, then keep start=0 to return to IDLE.
--   * flags(4 downto 0): [4] = NV, [3] = DZ(always 0), [2] = OF, [1] = UF, [0] = NX

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fp32mul_moore is
  generic (
    -- Rounding mode (IEEE-754):
    --  0 = RNE (round to nearest, ties to even)
    --  1 = RTZ (round toward zero)
    --  2 = RUP (round toward +Inf)
    --  3 = RDN (round toward -Inf)
    --  4 = RNA (round to nearest, ties away from zero / ties to max magnitude)
    ROUNDING_MODE : integer := 0;

    -- Denormals handling:
    --  0 = FULL     (gradual underflow, subnormals supported)
    --  1 = FTZ      (flush subnormal *results* to zero)
    --  2 = DAZ      (treat subnormal *inputs* as zero)
    --  3 = FTZ_DAZ  (both)
    DENORM_MODE   : integer := 0;

    -- Multiplier implementation:
    --  0 = COMB (single-cycle multiply)
    --  1 = ITER (iterative shift-add, 24 cycles)
    --  2 = DSP  (same as COMB, but may help DSP inference in some flows)
    MUL_IMPL      : integer := 0
  );
  port (
    clk   : in  std_logic;
    a     : in  std_logic_vector(31 downto 0);
    b     : in  std_logic_vector(31 downto 0);
    start : in  std_logic;

    result : out std_logic_vector(31 downto 0);
    done   : out std_logic;
    busy   : out std_logic;
    flags  : out std_logic_vector(4 downto 0)
  );
end entity;

architecture rtl of fp32mul_moore is
  -- classes
  constant CLS_ZERO    : integer := 0;
  constant CLS_SUBNORM : integer := 1;
  constant CLS_NORM    : integer := 2;
  constant CLS_INF     : integer := 3;
  constant CLS_NAN     : integer := 4;


  -- configuration encodings (match fp32mul_cfg_pkg)
  constant ROUND_RNE : integer := 0;
  constant ROUND_RTZ : integer := 1;
  constant ROUND_RUP : integer := 2;
  constant ROUND_RDN : integer := 3;
  constant ROUND_RNA : integer := 4;

  constant DENORM_FULL    : integer := 0;
  constant DENORM_FTZ     : integer := 1;
  constant DENORM_DAZ     : integer := 2;
  constant DENORM_FTZ_DAZ : integer := 3;

  constant MUL_COMB : integer := 0;
  constant MUL_ITER : integer := 1;
  constant MUL_DSP  : integer := 2;

  constant FTZ_EN : boolean := (DENORM_MODE = DENORM_FTZ) or (DENORM_MODE = DENORM_FTZ_DAZ);
  constant DAZ_EN : boolean := (DENORM_MODE = DENORM_DAZ) or (DENORM_MODE = DENORM_FTZ_DAZ);

  type state_t is (
    ST_IDLE, ST_UNPACK, ST_SPECIALS, ST_NORM_A, ST_NORM_B,
    ST_MUL, ST_MUL_ITER, ST_NORM_PROD, ST_SUB_SHIFT, ST_ROUND, ST_PACK, ST_DONE
  );

  function or_reduce(u : unsigned) return std_logic is
    variable r : std_logic := '0';
  begin
    for i in u'range loop
      r := r or std_logic(u(i));
    end loop;
    return r;
  end function;

  -- registers / state
  signal state : state_t := ST_IDLE;

  signal a_reg, b_reg : std_logic_vector(31 downto 0) := (others => '0');
  signal res_reg      : std_logic_vector(31 downto 0) := (others => '0');

  signal sign_a, sign_b : std_logic := '0';
  signal exp_a, exp_b   : unsigned(7 downto 0) := (others => '0');
  signal frac_a, frac_b : unsigned(22 downto 0) := (others => '0');

  signal cls_a, cls_b   : integer range 0 to 4 := CLS_ZERO;

  signal a_is_snan, b_is_snan : std_logic := '0';

  signal sign_res : std_logic := '0';

  signal exp_a_unb, exp_b_unb : integer := 0;
  signal exp_sum              : integer := 0;

  signal mant_a, mant_b : unsigned(23 downto 0) := (others => '0'); -- {hidden, frac}
  signal prod           : unsigned(47 downto 0) := (others => '0');
  signal prod_norm      : unsigned(47 downto 0) := (others => '0');

  -- iterative multiplier (MUL_ITER)
  signal mul_acc    : unsigned(47 downto 0) := (others => '0');
  signal mul_mcand  : unsigned(47 downto 0) := (others => '0');
  signal mul_mplier : unsigned(23 downto 0) := (others => '0');
  signal mul_cnt    : integer range 0 to 24 := 0;

  -- DSP inference hint (vendor-specific; ignored by most simulators)
  attribute use_dsp : string;
  attribute use_dsp of prod : signal is "yes";

  signal underflowed : std_logic := '0';
  signal sticky_sub  : std_logic := '0';

  -- rounding comb
  signal sig_keep   : unsigned(23 downto 0);
  signal guard_bit  : std_logic;
  signal round_bit  : std_logic;
  signal sticky_bit : std_logic;
  signal lsb_bit    : std_logic;
  signal inexact_bit : std_logic;
  signal round_inc  : std_logic;

  signal round_inc_u : unsigned(24 downto 0);
  signal sig_sum25   : unsigned(24 downto 0);

  signal sig_final : unsigned(23 downto 0) := (others => '0');

  -- flags
  signal f_nv : std_logic := '0';
  signal f_dz : std_logic := '0';
  signal f_of : std_logic := '0';
  signal f_uf : std_logic := '0';
  signal f_nx : std_logic := '0';

begin
  -- outputs
  result <= res_reg;

  done <= '1' when state = ST_DONE else '0';
  busy <= '1' when (state /= ST_IDLE and state /= ST_DONE) else '0';

  flags(0) <= f_nx; -- NX
  flags(1) <= f_uf; -- UF
  flags(2) <= f_of; -- OF
  flags(3) <= f_dz; -- DZ (always 0 here)
  flags(4) <= f_nv; -- NV

  -- rounding wires (same bit mapping as the Amaranth design)
  sig_keep  <= prod_norm(46 downto 23);
  guard_bit <= std_logic(prod_norm(22));
  round_bit <= std_logic(prod_norm(21));
  sticky_bit <= sticky_sub or or_reduce(prod_norm(20 downto 0));
  lsb_bit   <= std_logic(prod_norm(23));
  inexact_bit <= guard_bit or round_bit or sticky_bit;

  -- rounding increment selection
  process(all)
  begin
    round_inc <= '0';
    case ROUNDING_MODE is
      when ROUND_RNE =>
        round_inc <= guard_bit and (round_bit or sticky_bit or lsb_bit);
      when ROUND_RTZ =>
        round_inc <= '0';
      when ROUND_RUP =>
        if (sign_res = '0') and (inexact_bit = '1') then
          round_inc <= '1';
        end if;
      when ROUND_RDN =>
        if (sign_res = '1') and (inexact_bit = '1') then
          round_inc <= '1';
        end if;
      when ROUND_RNA =>
        round_inc <= guard_bit;
      when others =>
        round_inc <= guard_bit and (round_bit or sticky_bit or lsb_bit);
    end case;
  end process;
round_inc_u <= (0 => round_inc, others => '0');
  sig_sum25   <= ('0' & sig_keep) + round_inc_u;

  process(clk)
    variable exp_a_u, exp_b_u : unsigned(7 downto 0);
    variable frac_a_u, frac_b_u : unsigned(22 downto 0);
    variable frac_tmp : unsigned(22 downto 0);

    -- effective classes/fields (for DAZ)
    variable cls_a_eff, cls_b_eff : integer;
    variable exp_a_eff, exp_b_eff : unsigned(7 downto 0);
    variable frac_a_eff, frac_b_eff : unsigned(22 downto 0);
    variable prod_tmp : unsigned(47 downto 0);
    variable exp_tmp  : integer;

    variable carry : std_logic;

    -- iterative mul temps
    variable acc_v    : unsigned(47 downto 0);
    variable mcand_v  : unsigned(47 downto 0);
    variable mplier_v : unsigned(23 downto 0);
    variable cnt_v    : integer;
  begin
    if rising_edge(clk) then
      case state is
        when ST_IDLE =>
          if start = '1' then
            a_reg <= a;
            b_reg <= b;

            f_nv <= '0';
            f_dz <= '0';
            f_of <= '0';
            f_uf <= '0';
            f_nx <= '0';

            underflowed <= '0';
            sticky_sub <= '0';

            state <= ST_UNPACK;
          end if;

        when ST_UNPACK =>
          sign_a <= a_reg(31);
          sign_b <= b_reg(31);

          exp_a_u  := unsigned(a_reg(30 downto 23));
          exp_b_u  := unsigned(b_reg(30 downto 23));
          frac_a_u := unsigned(a_reg(22 downto 0));
          frac_b_u := unsigned(b_reg(22 downto 0));

          exp_a  <= exp_a_u;
          exp_b  <= exp_b_u;
          frac_a <= frac_a_u;
          frac_b <= frac_b_u;

          -- class A
          if (exp_a_u = 0) and (frac_a_u = 0) then
            cls_a <= CLS_ZERO;
          elsif (exp_a_u = 0) and (frac_a_u /= 0) then
            cls_a <= CLS_SUBNORM;
          elsif (exp_a_u = x"FF") and (frac_a_u = 0) then
            cls_a <= CLS_INF;
          elsif (exp_a_u = x"FF") and (frac_a_u /= 0) then
            cls_a <= CLS_NAN;
          else
            cls_a <= CLS_NORM;
          end if;

          -- class B
          if (exp_b_u = 0) and (frac_b_u = 0) then
            cls_b <= CLS_ZERO;
          elsif (exp_b_u = 0) and (frac_b_u /= 0) then
            cls_b <= CLS_SUBNORM;
          elsif (exp_b_u = x"FF") and (frac_b_u = 0) then
            cls_b <= CLS_INF;
          elsif (exp_b_u = x"FF") and (frac_b_u /= 0) then
            cls_b <= CLS_NAN;
          else
            cls_b <= CLS_NORM;
          end if;

          -- sNaN detection (quiet bit is MSB of fraction)
          if (exp_a_u = x"FF") and (frac_a_u /= 0) and (frac_a_u(22) = '0') then
            a_is_snan <= '1';
          else
            a_is_snan <= '0';
          end if;

          if (exp_b_u = x"FF") and (frac_b_u /= 0) and (frac_b_u(22) = '0') then
            b_is_snan <= '1';
          else
            b_is_snan <= '0';
          end if;

          state <= ST_SPECIALS;

        when ST_SPECIALS =>
          sign_res <= sign_a xor sign_b;

          -- apply DAZ (denormals-are-zero) to inputs if enabled
          cls_a_eff := cls_a;
          cls_b_eff := cls_b;
          exp_a_eff := exp_a;
          exp_b_eff := exp_b;
          frac_a_eff := frac_a;
          frac_b_eff := frac_b;

          if DAZ_EN and (cls_a = CLS_SUBNORM) then
            cls_a_eff := CLS_ZERO;
            exp_a_eff := (others => '0');
            frac_a_eff := (others => '0');
          end if;

          if DAZ_EN and (cls_b = CLS_SUBNORM) then
            cls_b_eff := CLS_ZERO;
            exp_b_eff := (others => '0');
            frac_b_eff := (others => '0');
          end if;

          if (a_is_snan = '1') or (b_is_snan = '1') then
            f_nv <= '1';
          end if;

          -- NaN handling: propagate payload, force quiet, sign=0
          if (cls_a_eff = CLS_NAN) or (cls_b_eff = CLS_NAN) then
            if (cls_a_eff = CLS_NAN) then
              frac_tmp := frac_a_eff;
            else
              frac_tmp := frac_b_eff;
            end if;
            frac_tmp(22) := '1'; -- force quiet

            res_reg(31) <= '0';
            res_reg(30 downto 23) <= (others => '1');
            res_reg(22 downto 0)  <= std_logic_vector(frac_tmp);

            state <= ST_DONE;

          -- invalid: Inf * 0
          elsif ((cls_a_eff = CLS_INF) and (cls_b_eff = CLS_ZERO)) or ((cls_b_eff = CLS_INF) and (cls_a_eff = CLS_ZERO)) then
            f_nv <= '1';
            res_reg <= x"7FC00000";
            state <= ST_DONE;

          -- Inf handling
          elsif (cls_a_eff = CLS_INF) or (cls_b_eff = CLS_INF) then
            res_reg(31) <= sign_res;
            res_reg(30 downto 23) <= (others => '1');
            res_reg(22 downto 0) <= (others => '0');
            state <= ST_DONE;

          -- zero handling
          elsif (cls_a_eff = CLS_ZERO) or (cls_b_eff = CLS_ZERO) then
            res_reg(31) <= sign_res;
            res_reg(30 downto 23) <= (others => '0');
            res_reg(22 downto 0) <= (others => '0');
            state <= ST_DONE;

          else
            -- setup A
            if cls_a_eff = CLS_NORM then
              exp_a_unb <= to_integer(exp_a_eff) - 127;
              mant_a <= unsigned('1' & std_logic_vector(frac_a_eff));
            else
              exp_a_unb <= -126;
              mant_a <= unsigned('0' & std_logic_vector(frac_a_eff));
            end if;

            -- setup B
            if cls_b_eff = CLS_NORM then
              exp_b_unb <= to_integer(exp_b_eff) - 127;
              mant_b <= unsigned('1' & std_logic_vector(frac_b_eff));
            else
              exp_b_unb <= -126;
              mant_b <= unsigned('0' & std_logic_vector(frac_b_eff));
            end if;

            if cls_a_eff = CLS_SUBNORM then
              state <= ST_NORM_A;
            elsif cls_b_eff = CLS_SUBNORM then
              state <= ST_NORM_B;
            else
              state <= ST_MUL;
            end if;
          end if;




        when ST_NORM_A =>
          if mant_a(23) = '0' then
            mant_a <= shift_left(mant_a, 1);
            exp_a_unb <= exp_a_unb - 1;
          else
            if cls_b = CLS_SUBNORM then
              state <= ST_NORM_B;
            else
              state <= ST_MUL;
            end if;
          end if;

        when ST_NORM_B =>
          if mant_b(23) = '0' then
            mant_b <= shift_left(mant_b, 1);
            exp_b_unb <= exp_b_unb - 1;
          else
            state <= ST_MUL;
          end if;

        when ST_MUL =>
          exp_sum <= exp_a_unb + exp_b_unb;
          underflowed <= '0';
          sticky_sub <= '0';
          if MUL_IMPL = MUL_ITER then
            mul_acc    <= (others => '0');
            mul_mcand  <= resize(mant_a, 48);
            mul_mplier <= mant_b;
            mul_cnt    <= 24;
            state      <= ST_MUL_ITER;
          else
            -- COMB/DSP: one-cycle multiply
            prod <= mant_a * mant_b;
            state <= ST_NORM_PROD;
          end if;

        when ST_MUL_ITER =>
          acc_v    := mul_acc;
          mcand_v  := mul_mcand;
          mplier_v := mul_mplier;
          cnt_v    := mul_cnt - 1;

          if mplier_v(0) = '1' then
            acc_v := acc_v + mcand_v;
          end if;

          mcand_v  := shift_left(mcand_v, 1);
          mplier_v := shift_right(mplier_v, 1);

          mul_acc    <= acc_v;
          mul_mcand  <= mcand_v;
          mul_mplier <= mplier_v;
          mul_cnt    <= cnt_v;

          if cnt_v = 0 then
            prod  <= acc_v;
            state <= ST_NORM_PROD;
          end if;

        when ST_NORM_PROD =>
          -- normalize product and adjust exponent
          prod_tmp := prod;
          exp_tmp  := exp_sum;

          if prod(47) = '1' then
            prod_tmp := shift_right(prod, 1);
            exp_tmp  := exp_sum + 1;
          end if;

          prod_norm <= prod_tmp;
          exp_sum   <= exp_tmp;

          -- exponent range handling (use adjusted exp_tmp)
          if exp_tmp < -150 then
            -- too small: flush to zero, UF=1, NX if anything nonzero
            res_reg(31) <= sign_res;
            res_reg(30 downto 23) <= (others => '0');
            res_reg(22 downto 0) <= (others => '0');

            f_uf <= '1';
            if prod_tmp /= 0 then
              f_nx <= '1';
            end if;

            state <= ST_DONE;

          elsif exp_tmp < -126 then
            if FTZ_EN then
              -- flush subnormal results to zero (FTZ)
              res_reg(31) <= sign_res;
              res_reg(30 downto 23) <= (others => '0');
              res_reg(22 downto 0) <= (others => '0');

              f_uf <= '1';
              if prod_tmp /= 0 then
                f_nx <= '1';
              end if;

              state <= ST_DONE;
            else
              underflowed <= '1';
              state <= ST_SUB_SHIFT;
            end if;

          else
            state <= ST_ROUND;
          end if;

        when ST_SUB_SHIFT =>
          if exp_sum < -126 then
            sticky_sub <= sticky_sub or std_logic(prod_norm(0));
            prod_norm <= shift_right(prod_norm, 1);
            exp_sum <= exp_sum + 1;
          else
            state <= ST_ROUND;
          end if;

        when ST_ROUND =>
          -- inexact flag
          if inexact_bit = '1' then
            f_nx <= f_nx or '1';
          end if;

          carry := std_logic(sig_sum25(24));
          if carry = '1' then
            sig_final <= sig_sum25(24 downto 1);
            exp_sum <= exp_sum + 1;
          else
            sig_final <= sig_sum25(23 downto 0);
          end if;

          state <= ST_PACK;

        when ST_PACK =>
          if exp_sum > 127 then
            -- overflow -> Inf
            f_of <= '1';
            f_nx <= '1';
            res_reg(31) <= sign_res;
            res_reg(30 downto 23) <= (others => '1');
            res_reg(22 downto 0) <= (others => '0');

          elsif underflowed = '1' then
            -- underflow/subnormal packing (exp field = 0 unless it rounded into min normal)
            if sig_final = to_unsigned(16#800000#, 24) then
              -- rounded up to 1.0 * 2^-126 => exp field 1, frac 0
              res_reg(31) <= sign_res;
              res_reg(30 downto 23) <= std_logic_vector(to_unsigned(1, 8));
              res_reg(22 downto 0) <= (others => '0');
            else
              res_reg(31) <= sign_res;
              res_reg(30 downto 23) <= (others => '0');
              res_reg(22 downto 0) <= std_logic_vector(sig_final(22 downto 0));
            end if;

            -- IEEE: UF only if inexact
            f_uf <= f_uf or f_nx;

          else
            -- normal
            res_reg(31) <= sign_res;
            res_reg(30 downto 23) <= std_logic_vector(to_unsigned(exp_sum + 127, 8));
            res_reg(22 downto 0) <= std_logic_vector(sig_final(22 downto 0));
          end if;

          state <= ST_DONE;

        when ST_DONE =>
          if start = '0' then
            state <= ST_IDLE;
          end if;

      end case;
    end if;
  end process;

end architecture;
