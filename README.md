# fp32mul

A VHDL-2008 implementation of an IEEE-754 binary32 multiplier as a **Moore FSM + registered datapath**.

## What’s included

- `src/fp32mul_moore.vhd` — multiplier core (Moore FSM + datapath; legacy `start/done` handshake)
- `src/fp32mul_stream.vhd` — **streaming wrapper** (`in_valid/in_ready`, `out_valid/out_ready`) + backpressure + request queue (2 entries) + output FIFO (2 entries)
- `src/fp32mul_cfg_pkg.vhd` — constants for modes (generic parameter codes)
- `tb/tb_fp32mul_stream.vhd` — a **pure VHDL** testbench for the streaming interface (directed vectors + backpressure (verifies that up to 2 results can accumulate in the output FIFO))
- `Makefile` — build/run via **GHDL**

## Interface (recommended): fp32mul_stream

Ports:

- input: `in_valid`, `in_ready`, `in_a`, `in_b`
- output: `out_valid`, `out_ready`, `out_result`, `out_flags` (**NV DZ OF UF NX** flags)

Handshake rule:

- An input transaction is accepted when `in_valid=1` and `in_ready=1` on a rising clock edge.
- A result is considered **accepted by the consumer** when `out_valid=1` and `out_ready=1` on a rising clock edge.

Internally there is a 2-entry FIFO on the input and a 2-entry FIFO on the output — you can “push” requests, and the core can finish another operation even if the output consumer stalls temporarily.

## Legacy core interface: fp32mul_moore

Still kept in the project and can be useful for FSM debugging: pulse `start` for 1 cycle, wait for `done`.

## Modes: rounding and denormals

Modes apply to both `fp32mul_moore` and `fp32mul_stream` (the wrapper just passes generics through).

```vhdl
  generic (
    ROUNDING_MODE : integer := 0; -- 0..4
    DENORM_MODE   : integer := 0; -- 0..3

    -- 0=COMB, 1=ITER, 2=DSP
    MUL_IMPL      : integer := 0
  );
```

Mode codes (see `src/fp32mul_cfg_pkg.vhd`):

### Rounding (`ROUNDING_MODE`)

- `ROUND_RNE = 0` — round to nearest, ties to even (**default**, IEEE)
- `ROUND_RTZ = 1` — round toward zero
- `ROUND_RUP = 2` — round toward +Inf
- `ROUND_RDN = 3` — round toward -Inf
- `ROUND_RNA = 4` — round to nearest, ties away from zero (ties to max magnitude)

### Denormals (`DENORM_MODE`)

- `DENORM_FULL = 0` — subnormal support (gradual underflow), as-is (**default**)
- `DENORM_FTZ  = 1` — **FTZ**: subnormal *results* are forced to 0 (+UF, +NX if the value is non-zero)
- `DENORM_DAZ  = 2` — **DAZ**: subnormal *inputs* are treated as zero
- `DENORM_FTZ_DAZ = 3` — FTZ + DAZ

Instantiation example:

```vhdl
u_mul : entity work.fp32mul_stream
  generic map (
    ROUNDING_MODE => ROUND_RUP,
    DENORM_MODE   => DENORM_FTZ,
    MUL_IMPL      => MUL_ITER
  )
  port map (
    clk => clk,
    in_valid   => in_valid,
    in_ready   => in_ready,
    in_a       => in_a,
    in_b       => in_b,
    out_valid  => out_valid,
    out_ready  => out_ready,
    out_result => out_result,
    out_flags  => out_flags
  );
```

## How to run

Requires: `ghdl` and (optionally) `gtkwave`.

```bash
make
```

This will:
- analyze VHDL
- build the testbench
- run it
- generate `wave.vcd`

Run two multiplier variants (COMB/ITER):

```bash
make run_all
```

Or select manually:

```bash
make TB_MUL_IMPL=0   # COMB
make TB_MUL_IMPL=1   # ITER
make TB_MUL_IMPL=2   # DSP (in simulation behaves like COMB)
```

Open waveforms:

```bash
gtkwave wave.vcd
```

## About the tests

The testbench (`tb_fp32mul_stream`) checks:
- basic exact multiplications
- signed zeros
- infinities
- Inf * 0 -> NaN + NV
- qNaN/sNaN propagation (quiet + payload) + NV for sNaN
- overflow -> Inf + OF + NX
- underflow flush-to-zero -> UF + NX (exponent branch < -150)
- exact subnormal and subnormal -> normal (in FULL mode)
- inexact rounding examples (NX)
- `out_valid/out_ready` behavior (backpressure): when `out_ready=0` the output must remain stable
