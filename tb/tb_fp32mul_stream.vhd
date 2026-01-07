library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.env.all;

use work.fp32mul_cfg_pkg.all;

entity tb_fp32mul_stream is
  generic (
    MUL_IMPL_G : integer := MUL_COMB
  );
end entity;

architecture sim of tb_fp32mul_stream is
  constant CLK_PERIOD : time := 10 ns;
  constant MAX_CYCLES : natural := 50000;

  signal clk : std_logic := '0';

  -- streaming interface
  signal in_valid : std_logic := '0';
  signal in_ready : std_logic;
  signal in_a     : std_logic_vector(31 downto 0) := (others => '0');
  signal in_b     : std_logic_vector(31 downto 0) := (others => '0');

  signal out_valid  : std_logic;
  signal out_ready  : std_logic := '1';
  signal out_result : std_logic_vector(31 downto 0);
  signal out_flags  : std_logic_vector(4 downto 0);

  signal prod_done : std_logic := '0';
  signal cons_done : std_logic := '0';

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

    -- underflow flush: min subnormal * min subnormal -> 0, UF=1, NX=1
    (x"00000001", x"00000001", x"00000000", mkflags('0','0','0','1','1')),

    -- subnormal exact result: min normal * 0.5 = 2^-127 => subnormal 0x00400000
    (x"00800000", x"3F000000", x"00400000", mkflags('0','0','0','0','0')),

    -- subnormal passthrough: subnormal * 1.0 = same
    (x"00012345", x"3F800000", x"00012345", mkflags('0','0','0','0','0')),

    -- subnormal -> normal transition: largest subnormal * 2.0
    (x"007FFFFF", x"40000000", x"00FFFFFE", mkflags('0','0','0','0','0')),

    -- inexact rounding: (1 + 2^-23) * (1 + 2^-23) => 0x3F800002, NX=1
    (x"3F800001", x"3F800001", x"3F800002", mkflags('0','0','0','0','1'))
  );

begin
  clk <= not clk after CLK_PERIOD / 2;

  dut : entity work.fp32mul_stream
    generic map (
      ROUNDING_MODE => ROUND_RNE,
      DENORM_MODE   => DENORM_FULL,
      MUL_IMPL      => MUL_IMPL_G
    )
    port map (
      clk        => clk,
      in_valid   => in_valid,
      in_ready   => in_ready,
      in_a       => in_a,
      in_b       => in_b,
      out_valid  => out_valid,
      out_ready  => out_ready,
      out_result => out_result,
      out_flags  => out_flags
    );

  -- Producer: keeps in_valid high, advances only when in_ready.
  producer_p : process
    variable i : natural := 0;
    variable cycles : natural := 0;
    variable saw_inready0 : boolean := false;
  begin
    -- settle
    for k in 0 to 3 loop
      wait until rising_edge(clk);
    end loop;

    in_valid <= '1';
    while i < tvs'length loop
      in_a <= tvs(i).a;
      in_b <= tvs(i).b;
      wait until rising_edge(clk);

      if in_ready = '0' then
        saw_inready0 := true;
      end if;

      if in_ready = '1' then
        i := i + 1;
      end if;

      cycles := cycles + 1;
      assert cycles < MAX_CYCLES
        report "Timeout while sending inputs"
        severity failure;
    end loop;

    in_valid <= '0';
    prod_done <= '1';

    assert saw_inready0
      report "Did not observe in_ready deassertion; FIFO/backpressure might not be exercised"
      severity warning;

    wait;
  end process;

  -- Consumer: checks outputs in order; includes a stronger backpressure test:
-- hold out_ready low after the FIRST output appears long enough for a SECOND
-- output to be produced and queued in the output FIFO.
consumer_p : process
  variable idx : natural := 0;
  variable cycles : natural;
  variable hold_res : std_logic_vector(31 downto 0);
  variable hold_flags : std_logic_vector(4 downto 0);
  constant HOLD_CYCLES : natural := 200;
begin
  out_ready <= '1';

  -- ===== Wait for first output, then apply backpressure =====
  cycles := 0;
  while out_valid /= '1' loop
    wait until rising_edge(clk);
    cycles := cycles + 1;
    assert cycles < MAX_CYCLES
      report "Timeout waiting first out_valid"
      severity failure;
  end loop;

  -- Stop consuming BEFORE the next rising edge.
  out_ready <= '0';

  -- Check first output matches tvs(0) while stalled.
  assert out_result = tvs(0).r
    report "Result mismatch idx=0 got=0x" & to_hstring(out_result) & " exp=0x" & to_hstring(tvs(0).r)
    severity failure;

  assert out_flags = tvs(0).f
    report "Flags mismatch idx=0 got=" & slv_to_bin(out_flags) & " exp=" & slv_to_bin(tvs(0).f)
    severity failure;

  hold_res := out_result;
  hold_flags := out_flags;

  -- Hold for a while: out_valid must stay asserted and output must not change.
  for t in 0 to HOLD_CYCLES-1 loop
    wait until rising_edge(clk);
    assert out_valid = '1'
      report "out_valid dropped under backpressure"
      severity failure;
    assert out_result = hold_res
      report "out_result changed under backpressure"
      severity failure;
    assert out_flags = hold_flags
      report "out_flags changed under backpressure"
      severity failure;
  end loop;

  -- Release backpressure and consume the first output.
  out_ready <= '1';
  wait until rising_edge(clk);
  idx := 1;

  -- Immediately after consuming the first output, the SECOND should already be queued.
  assert out_valid = '1'
    report "Expected second output queued (output FIFO depth=2), but out_valid deasserted"
    severity failure;

  assert out_result = tvs(1).r
    report "Result mismatch idx=1 got=0x" & to_hstring(out_result) & " exp=0x" & to_hstring(tvs(1).r)
    severity failure;

  assert out_flags = tvs(1).f
    report "Flags mismatch idx=1 got=" & slv_to_bin(out_flags) & " exp=" & slv_to_bin(tvs(1).f)
    severity failure;

  -- Consume second output.
  wait until rising_edge(clk);
  idx := 2;

  -- ===== Consume remaining outputs normally =====
  while idx < tvs'length loop
    cycles := 0;
    while out_valid /= '1' loop
      wait until rising_edge(clk);
      cycles := cycles + 1;
      assert cycles < MAX_CYCLES
        report "Timeout waiting out_valid idx=" & integer'image(idx)
        severity failure;
    end loop;

    assert out_result = tvs(idx).r
      report "Result mismatch idx=" & integer'image(idx) &
             " got=0x" & to_hstring(out_result) & " exp=0x" & to_hstring(tvs(idx).r)
      severity failure;

    assert out_flags = tvs(idx).f
      report "Flags mismatch idx=" & integer'image(idx) &
             " got=" & slv_to_bin(out_flags) & " exp=" & slv_to_bin(tvs(idx).f)
      severity failure;

    wait until rising_edge(clk);
    idx := idx + 1;
  end loop;

  cons_done <= '1';
  report "All stream tests passed (MUL_IMPL=" & integer'image(MUL_IMPL_G) & ")" severity note;
  wait;
end process;


  -- Stop sim when done.
  finisher_p : process
  begin
    wait until (prod_done = '1') and (cons_done = '1');
    report "DONE" severity note;
    stop(0);
  end process;

end architecture;
