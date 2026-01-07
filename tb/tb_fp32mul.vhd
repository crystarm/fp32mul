library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.fp32mul_cfg_pkg.all;

entity tb_fp32mul is
end entity;

architecture sim of tb_fp32mul is
  constant CLK_PERIOD : time := 10 ns;
  constant MAX_CYCLES : natural := 500;

  signal clk : std_logic := '0';

  -- DUT0 (default: RNE + FULL)
  signal a0     : std_logic_vector(31 downto 0) := (others => '0');
  signal b0     : std_logic_vector(31 downto 0) := (others => '0');
  signal start0 : std_logic := '0';
  signal result0 : std_logic_vector(31 downto 0);
  signal done0   : std_logic;
  signal busy0   : std_logic;
  signal flags0  : std_logic_vector(4 downto 0);

  -- DUT_FTZ (RNE + FTZ)
  signal a_ftz     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_ftz     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_ftz : std_logic := '0';
  signal result_ftz : std_logic_vector(31 downto 0);
  signal done_ftz   : std_logic;
  signal busy_ftz   : std_logic;
  signal flags_ftz  : std_logic_vector(4 downto 0);

  -- DUT_DAZ (RNE + DAZ)
  signal a_daz     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_daz     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_daz : std_logic := '0';
  signal result_daz : std_logic_vector(31 downto 0);
  signal done_daz   : std_logic;
  signal busy_daz   : std_logic;
  signal flags_daz  : std_logic_vector(4 downto 0);

  -- Rounding-mode DUTs (FULL denorms)
  signal a_rtz     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_rtz     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_rtz : std_logic := '0';
  signal result_rtz : std_logic_vector(31 downto 0);
  signal done_rtz   : std_logic;
  signal busy_rtz   : std_logic;
  signal flags_rtz  : std_logic_vector(4 downto 0);

  signal a_rup     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_rup     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_rup : std_logic := '0';
  signal result_rup : std_logic_vector(31 downto 0);
  signal done_rup   : std_logic;
  signal busy_rup   : std_logic;
  signal flags_rup  : std_logic_vector(4 downto 0);

  signal a_rdn     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_rdn     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_rdn : std_logic := '0';
  signal result_rdn : std_logic_vector(31 downto 0);
  signal done_rdn   : std_logic;
  signal busy_rdn   : std_logic;
  signal flags_rdn  : std_logic_vector(4 downto 0);

  signal a_rna     : std_logic_vector(31 downto 0) := (others => '0');
  signal b_rna     : std_logic_vector(31 downto 0) := (others => '0');
  signal start_rna : std_logic := '0';
  signal result_rna : std_logic_vector(31 downto 0);
  signal done_rna   : std_logic;
  signal busy_rna   : std_logic;
  signal flags_rna  : std_logic_vector(4 downto 0);

  function mkflags(nv, dz, ofl, uf, nx : std_logic) return std_logic_vector is
    variable v : std_logic_vector(4 downto 0);
  begin
    v(4) := nv;
    v(3) := dz;
    v(2) := ofl;
    v(1) := uf;
    v(0) := nx;
    return v;
  end function;

  function slv_to_bin(slv : std_logic_vector) return string is
    variable s : string(1 to slv'length);
    variable k : integer := 1;
    variable c : character;
  begin
    for i in slv'reverse_range loop
      case slv(i) is
        when '0' => c := '0';
        when '1' => c := '1';
        when 'U' => c := 'U';
        when 'X' => c := 'X';
        when 'Z' => c := 'Z';
        when 'W' => c := 'W';
        when 'L' => c := 'L';
        when 'H' => c := 'H';
        when '-' => c := '-';
      end case;
      s(k) := c;
      k := k + 1;
    end loop;
    return s;
  end function;

  type tv_t is record
    a : std_logic_vector(31 downto 0);
    b : std_logic_vector(31 downto 0);
    r : std_logic_vector(31 downto 0);
    f : std_logic_vector(4 downto 0);
  end record;

  type tv_arr_t is array (natural range <>) of tv_t;

  -- Default-mode vectors (RNE + FULL)
  constant tvs : tv_arr_t := (
    -- basics (exact)
    (x"3F800000", x"3F800000", x"3F800000", mkflags('0','0','0','0','0')), -- 1.0 * 1.0 = 1.0
    (x"40000000", x"40400000", x"40C00000", mkflags('0','0','0','0','0')), -- 2.0 * 3.0 = 6.0
    (x"BF800000", x"40000000", x"C0000000", mkflags('0','0','0','0','0')), -- -1.0 * 2.0 = -2.0
    (x"C0400000", x"40800000", x"C1400000", mkflags('0','0','0','0','0')), -- -3.0 * 4.0 = -12.0
    (x"3F000000", x"3F000000", x"3E800000", mkflags('0','0','0','0','0')), -- 0.5 * 0.5 = 0.25

    -- signed zero behavior
    (x"00000000", x"80000000", x"80000000", mkflags('0','0','0','0','0')), -- +0 * -0 = -0
    (x"80000000", x"80000000", x"00000000", mkflags('0','0','0','0','0')), -- -0 * -0 = +0
    (x"80000000", x"40400000", x"80000000", mkflags('0','0','0','0','0')), -- -0 * 3.0 = -0

    -- infinities
    (x"FF800000", x"40000000", x"FF800000", mkflags('0','0','0','0','0')), -- -Inf * 2.0 = -Inf
    (x"FF800000", x"C0000000", x"7F800000", mkflags('0','0','0','0','0')), -- -Inf * -2.0 = +Inf

    -- invalid: Inf * 0 -> qNaN (forced positive), NV=1
    (x"7F800000", x"00000000", x"7FC00000", mkflags('1','0','0','0','0')),

    -- NaN propagation (payload kept, quiet forced, sign forced 0)
    (x"7FC12345", x"3F800000", x"7FC12345", mkflags('0','0','0','0','0')), -- qNaN stays qNaN
    (x"FFC00001", x"3F800000", x"7FC00001", mkflags('0','0','0','0','0')), -- qNaN sign forced to 0

    -- sNaN: quiet it + NV=1
    (x"7FA12345", x"3F800000", x"7FE12345", mkflags('1','0','0','0','0')),
    (x"3F800000", x"7FA0BEEF", x"7FE0BEEF", mkflags('1','0','0','0','0')),

    -- overflow: max finite * 2 -> +Inf, OF=1, NX=1
    (x"7F7FFFFF", x"40000000", x"7F800000", mkflags('0','0','1','0','1')),

    -- underflow flush: min subnormal * min subnormal -> 0, UF=1, NX=1 (design-specific)
    (x"00000001", x"00000001", x"00000000", mkflags('0','0','0','1','1')),

    -- subnormal exact result: min normal * 0.5 = 2^-127 => subnormal 0x00400000, no UF/NX
    (x"00800000", x"3F000000", x"00400000", mkflags('0','0','0','0','0')),

    -- subnormal passthrough: subnormal * 1.0 = same
    (x"00012345", x"3F800000", x"00012345", mkflags('0','0','0','0','0')),

    -- subnormal -> normal transition: largest subnormal * 2.0
    (x"007FFFFF", x"40000000", x"00FFFFFE", mkflags('0','0','0','0','0')),

    -- inexact rounding: (1 + 2^-23) * (1 + 2^-23) => 0x3F800002, NX=1
    (x"3F800001", x"3F800001", x"3F800002", mkflags('0','0','0','0','1')),

    -- rounding tie-case (guard=1, round=0, sticky=0, lsb=0): RNE keeps, NX=1
    (x"3FA6E800", x"3FB86800", x"3FF0751C", mkflags('0','0','0','0','1')),
    (x"BFA6E800", x"3FB86800", x"BFF0751C", mkflags('0','0','0','0','1'))
  );

  procedure run_one_on(
    signal a_s     : out std_logic_vector(31 downto 0);
    signal b_s     : out std_logic_vector(31 downto 0);
    signal start_s : out std_logic;
    signal result_s : in std_logic_vector(31 downto 0);
    signal done_s   : in std_logic;
    signal busy_s   : in std_logic;
    signal flags_s  : in std_logic_vector(4 downto 0);
    constant t : tv_t;
    constant idx : natural;
    constant tag : string
  ) is
    variable cycles : natural := 0;
  begin
    -- wait for IDLE (done=0, busy=0)
    start_s <= '0';
    wait until rising_edge(clk);
    while (done_s = '1') or (busy_s = '1') loop
      wait until rising_edge(clk);
    end loop;

    -- drive inputs and pulse start for exactly 1 cycle
    a_s <= t.a;
    b_s <= t.b;
    start_s <= '1';
    wait until rising_edge(clk);
    start_s <= '0';

    -- wait for done with timeout
    cycles := 0;
    while done_s /= '1' loop
      wait until rising_edge(clk);
      cycles := cycles + 1;
      assert cycles < MAX_CYCLES
        report "Timeout waiting done (" & tag & ") at test #" & integer'image(idx)
        severity failure;
    end loop;

    wait for 1 ns;

    assert result_s = t.r
      report "Result mismatch (" & tag & ") at test #" & integer'image(idx) &
             " got=0x" & to_hstring(result_s) & " exp=0x" & to_hstring(t.r)
      severity failure;

    assert flags_s = t.f
      report "Flags mismatch (" & tag & ") at test #" & integer'image(idx) &
             " got=" & slv_to_bin(flags_s) & " exp=" & slv_to_bin(t.f)
      severity failure;

    -- return to IDLE (ST_DONE -> ST_IDLE when start=0)
    wait until rising_edge(clk);
    assert done_s = '0'
      report "DUT did not leave DONE (" & tag & ") at test #" & integer'image(idx)
      severity failure;
  end procedure;

begin
  clk <= not clk after CLK_PERIOD / 2;

  dut_default : entity work.fp32mul_moore
    port map (
      clk    => clk,
      a      => a0,
      b      => b0,
      start  => start0,
      result => result0,
      done   => done0,
      busy   => busy0,
      flags  => flags0
    );

  dut_ftz : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RNE,
      DENORM_MODE   => DENORM_FTZ
    )
    port map (
      clk    => clk,
      a      => a_ftz,
      b      => b_ftz,
      start  => start_ftz,
      result => result_ftz,
      done   => done_ftz,
      busy   => busy_ftz,
      flags  => flags_ftz
    );

  dut_daz : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RNE,
      DENORM_MODE   => DENORM_DAZ
    )
    port map (
      clk    => clk,
      a      => a_daz,
      b      => b_daz,
      start  => start_daz,
      result => result_daz,
      done   => done_daz,
      busy   => busy_daz,
      flags  => flags_daz
    );

  dut_rtz : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RTZ,
      DENORM_MODE   => DENORM_FULL
    )
    port map (
      clk    => clk,
      a      => a_rtz,
      b      => b_rtz,
      start  => start_rtz,
      result => result_rtz,
      done   => done_rtz,
      busy   => busy_rtz,
      flags  => flags_rtz
    );

  dut_rup : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RUP,
      DENORM_MODE   => DENORM_FULL
    )
    port map (
      clk    => clk,
      a      => a_rup,
      b      => b_rup,
      start  => start_rup,
      result => result_rup,
      done   => done_rup,
      busy   => busy_rup,
      flags  => flags_rup
    );

  dut_rdn : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RDN,
      DENORM_MODE   => DENORM_FULL
    )
    port map (
      clk    => clk,
      a      => a_rdn,
      b      => b_rdn,
      start  => start_rdn,
      result => result_rdn,
      done   => done_rdn,
      busy   => busy_rdn,
      flags  => flags_rdn
    );

  dut_rna : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUND_RNA,
      DENORM_MODE   => DENORM_FULL
    )
    port map (
      clk    => clk,
      a      => a_rna,
      b      => b_rna,
      start  => start_rna,
      result => result_rna,
      done   => done_rna,
      busy   => busy_rna,
      flags  => flags_rna
    );

  stim : process
    constant tv_ftz : tv_arr_t := (
      -- exact subnormal in FULL becomes flushed to zero in FTZ.
      -- (min normal) * 0.5 = 2^-127 should be non-zero subnormal, but FTZ forces zero.
      (x"00800000", x"3F000000", x"00000000", mkflags('0','0','0','1','1'))
    );

    constant tv_daz : tv_arr_t := (
      -- DAZ: subnormal inputs are treated as zero (no extra flags here).
      (x"00012345", x"3F800000", x"00000000", mkflags('0','0','0','0','0')),
      (x"80012345", x"3F800000", x"80000000", mkflags('0','0','0','0','0')),
      -- Inf * subnormal -> Inf * 0 -> invalid (NV=1)
      (x"7F800000", x"00012345", x"7FC00000", mkflags('1','0','0','0','0'))
    );

    -- tie-case operands:
    --  a = 0x3FA6E800, b = 0x3FB86800, exact product sits exactly at 0.5 ulp boundary.
    constant tv_tie_pos : tv_t := (x"3FA6E800", x"3FB86800", x"00000000", mkflags('0','0','0','0','1'));
    constant tv_tie_neg : tv_t := (x"BFA6E800", x"3FB86800", x"00000000", mkflags('0','0','0','0','1'));

    variable idx : natural := 0;

    procedure run_rounding_suite(constant tpos, tneg : tv_t) is
      variable t : tv_t;
    begin
      -- positive tie-case
      t := tpos;

      -- RTZ: trunc => same as RNE here (0x3FF0751C)
      t.r := x"3FF0751C";
      run_one_on(a_rtz, b_rtz, start_rtz, result_rtz, done_rtz, busy_rtz, flags_rtz, t, 0, "RTZ(+tie)");

      -- RUP: toward +Inf => increments => 0x3FF0751D
      t.r := x"3FF0751D";
      run_one_on(a_rup, b_rup, start_rup, result_rup, done_rup, busy_rup, flags_rup, t, 0, "RUP(+tie)");

      -- RDN: toward -Inf (positive result) => no increment => 0x3FF0751C
      t.r := x"3FF0751C";
      run_one_on(a_rdn, b_rdn, start_rdn, result_rdn, done_rdn, busy_rdn, flags_rdn, t, 0, "RDN(+tie)");

      -- RNA: ties away => increments on tie => 0x3FF0751D
      t.r := x"3FF0751D";
      run_one_on(a_rna, b_rna, start_rna, result_rna, done_rna, busy_rna, flags_rna, t, 0, "RNA(+tie)");

      -- negative tie-case (result negative)
      t := tneg;

      -- RTZ: trunc => 0xBFF0751C
      t.r := x"BFF0751C";
      run_one_on(a_rtz, b_rtz, start_rtz, result_rtz, done_rtz, busy_rtz, flags_rtz, t, 1, "RTZ(-tie)");

      -- RUP: toward +Inf (negative result) => no increment => 0xBFF0751C
      t.r := x"BFF0751C";
      run_one_on(a_rup, b_rup, start_rup, result_rup, done_rup, busy_rup, flags_rup, t, 1, "RUP(-tie)");

      -- RDN: toward -Inf (negative result) => increments => 0xBFF0751D
      t.r := x"BFF0751D";
      run_one_on(a_rdn, b_rdn, start_rdn, result_rdn, done_rdn, busy_rdn, flags_rdn, t, 1, "RDN(-tie)");

      -- RNA: ties away => increments => 0xBFF0751D
      t.r := x"BFF0751D";
      run_one_on(a_rna, b_rna, start_rna, result_rna, done_rna, busy_rna, flags_rna, t, 1, "RNA(-tie)");
    end procedure;

  begin
    -- settle
    for i in 0 to 5 loop
      wait until rising_edge(clk);
    end loop;

    -- default vectors
    for i in tvs'range loop
      run_one_on(a0, b0, start0, result0, done0, busy0, flags0, tvs(i), i, "DEFAULT");
    end loop;

    -- FTZ mode vectors
    for i in tv_ftz'range loop
      run_one_on(a_ftz, b_ftz, start_ftz, result_ftz, done_ftz, busy_ftz, flags_ftz, tv_ftz(i), i, "FTZ");
    end loop;

    -- DAZ mode vectors
    for i in tv_daz'range loop
      run_one_on(a_daz, b_daz, start_daz, result_daz, done_daz, busy_daz, flags_daz, tv_daz(i), i, "DAZ");
    end loop;

    -- rounding suite (FULL denorms)
    run_rounding_suite(tv_tie_pos, tv_tie_neg);

    report "ALL TESTS PASSED" severity note;
    wait;
  end process;

end architecture;
