-- fp32mul_stream.vhd
-- Streaming wrapper for fp32mul_moore:
--   * in_valid/in_ready + out_valid/out_ready (backpressure)
--   * 2-entry input FIFO to enqueue requests while one is in flight
--   * 2-entry output FIFO so the core can finish one extra operation while the
--     consumer is stalled (true decoupling producer/consumer)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.fp32mul_cfg_pkg.all;

entity fp32mul_stream is
  generic (
    ROUNDING_MODE : integer := ROUND_RNE;
    DENORM_MODE   : integer := DENORM_FULL;
    MUL_IMPL      : integer := MUL_COMB
  );
  port (
    clk : in std_logic;

    in_valid : in  std_logic;
    in_ready : out std_logic;
    in_a     : in  std_logic_vector(31 downto 0);
    in_b     : in  std_logic_vector(31 downto 0);

    out_valid  : out std_logic;
    out_ready  : in  std_logic;
    out_result : out std_logic_vector(31 downto 0);
    out_flags  : out std_logic_vector(4 downto 0)
  );
end entity;

architecture rtl of fp32mul_stream is
  constant IN_DEPTH  : integer := 2;
  constant OUT_DEPTH : integer := 2;

  type slv32_in_arr_t is array (0 to IN_DEPTH-1) of std_logic_vector(31 downto 0);
  signal fifo_a : slv32_in_arr_t := (others => (others => '0'));
  signal fifo_b : slv32_in_arr_t := (others => (others => '0'));

  signal rd_ptr : integer range 0 to IN_DEPTH-1 := 0;
  signal wr_ptr : integer range 0 to IN_DEPTH-1 := 0;
  signal count  : integer range 0 to IN_DEPTH := 0;

  -- core interface
  signal core_a     : std_logic_vector(31 downto 0) := (others => '0');
  signal core_b     : std_logic_vector(31 downto 0) := (others => '0');
  signal core_start : std_logic := '0';

  signal core_result : std_logic_vector(31 downto 0);
  signal core_done   : std_logic;
  signal core_busy   : std_logic;
  signal core_flags  : std_logic_vector(4 downto 0);

  -- output FIFO
  type slv32_out_arr_t is array (0 to OUT_DEPTH-1) of std_logic_vector(31 downto 0);
  type slv5_out_arr_t  is array (0 to OUT_DEPTH-1) of std_logic_vector(4 downto 0);

  signal out_fifo_result : slv32_out_arr_t := (others => (others => '0'));
  signal out_fifo_flags  : slv5_out_arr_t  := (others => (others => '0'));
  signal out_rd_ptr      : integer range 0 to OUT_DEPTH-1 := 0;
  signal out_wr_ptr      : integer range 0 to OUT_DEPTH-1 := 0;
  signal out_count       : integer range 0 to OUT_DEPTH := 0;

begin
  -- combinational ready/valid
  in_ready  <= '1' when count < IN_DEPTH else '0';
  out_valid <= '1' when out_count > 0 else '0';

  out_result <= out_fifo_result(out_rd_ptr);
  out_flags  <= out_fifo_flags(out_rd_ptr);

  dut : entity work.fp32mul_moore
    generic map (
      ROUNDING_MODE => ROUNDING_MODE,
      DENORM_MODE   => DENORM_MODE,
      MUL_IMPL      => MUL_IMPL
    )
    port map (
      clk    => clk,
      a      => core_a,
      b      => core_b,
      start  => core_start,
      result => core_result,
      done   => core_done,
      busy   => core_busy,
      flags  => core_flags
    );

  process(clk)
    variable do_in_push : boolean;
    variable do_in_pop  : boolean;

    variable do_out_pop  : boolean;
    variable do_out_push : boolean;

    variable rd_next  : integer range 0 to IN_DEPTH-1;
    variable wr_next  : integer range 0 to IN_DEPTH-1;
    variable cnt_next : integer range 0 to IN_DEPTH;

    variable out_rd_next  : integer range 0 to OUT_DEPTH-1;
    variable out_wr_next  : integer range 0 to OUT_DEPTH-1;
    variable out_cnt_next : integer range 0 to OUT_DEPTH;

    variable out_space_after_pop : integer range 0 to OUT_DEPTH;
  begin
    if rising_edge(clk) then
      -- default: no start pulse
      core_start <= '0';

      -- ==== output FIFO pop/push ====
      do_out_pop := (out_ready = '1') and (out_count > 0);
      -- push is requested when core finishes
      -- (allowed if there is space, considering a pop in the same cycle)
      if do_out_pop then
        out_space_after_pop := OUT_DEPTH - (out_count - 1);
      else
        out_space_after_pop := OUT_DEPTH - out_count;
      end if;

      do_out_push := (core_done = '1') and (out_space_after_pop > 0);

      if (core_done = '1') and (out_space_after_pop = 0) then
        assert false report "Output FIFO overflow (consumer stalled too long)" severity failure;
      end if;

      out_rd_next  := out_rd_ptr;
      out_wr_next  := out_wr_ptr;
      out_cnt_next := out_count;

      if do_out_pop then
        out_rd_next := (out_rd_ptr + 1) mod OUT_DEPTH;
        out_cnt_next := out_cnt_next - 1;
      end if;

      if do_out_push then
        out_fifo_result(out_wr_ptr) <= core_result;
        out_fifo_flags(out_wr_ptr)  <= core_flags;
        out_wr_next := (out_wr_ptr + 1) mod OUT_DEPTH;
        out_cnt_next := out_cnt_next + 1;
      end if;

      out_rd_ptr <= out_rd_next;
      out_wr_ptr <= out_wr_next;
      out_count  <= out_cnt_next;

      -- ==== input FIFO push/pop ====
      do_in_push := (in_valid = '1') and (in_ready = '1');

      -- launch a new core operation when idle and input FIFO is non-empty,
      -- and output FIFO is not full (considering a possible pop this cycle)
      do_in_pop := (core_busy = '0') and (core_done = '0') and (count > 0) and (out_cnt_next < OUT_DEPTH);

      rd_next := rd_ptr;
      wr_next := wr_ptr;
      cnt_next := count;

      if do_in_push then
        fifo_a(wr_ptr) <= in_a;
        fifo_b(wr_ptr) <= in_b;
        wr_next := (wr_ptr + 1) mod IN_DEPTH;
        cnt_next := cnt_next + 1;
      end if;

      if do_in_pop then
        core_a <= fifo_a(rd_ptr);
        core_b <= fifo_b(rd_ptr);
        core_start <= '1';
        rd_next := (rd_ptr + 1) mod IN_DEPTH;
        cnt_next := cnt_next - 1;
      end if;

      rd_ptr <= rd_next;
      wr_ptr <= wr_next;
      count  <= cnt_next;
    end if;
  end process;

end architecture;
