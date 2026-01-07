-- fp32mul_cfg_pkg.vhd
-- Shared encodings for generics (rounding/denorm modes)

library ieee;
use ieee.std_logic_1164.all;

package fp32mul_cfg_pkg is
  -- Rounding modes
  constant ROUND_RNE : integer := 0; -- round to nearest, ties to even
  constant ROUND_RTZ : integer := 1; -- round toward zero
  constant ROUND_RUP : integer := 2; -- round toward +Inf
  constant ROUND_RDN : integer := 3; -- round toward -Inf
  constant ROUND_RNA : integer := 4; -- round to nearest, ties away from zero (ties to max magnitude)

  -- Denorm handling modes
  constant DENORM_FULL    : integer := 0; -- gradual underflow, subnormals supported
  constant DENORM_FTZ     : integer := 1; -- flush subnormal results to zero
  constant DENORM_DAZ     : integer := 2; -- treat subnormal inputs as zero
  constant DENORM_FTZ_DAZ : integer := 3; -- both FTZ and DAZ

  -- Multiplier implementation
  constant MUL_COMB : integer := 0; -- single-cycle combinational multiply
  constant MUL_ITER : integer := 1; -- iterative shift-add (24 cycles)
  constant MUL_DSP  : integer := 2; -- hint to infer DSP blocks (vendor-specific)
end package;

package body fp32mul_cfg_pkg is
end package body;
