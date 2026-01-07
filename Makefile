GHDL ?= ghdl
STD  ?= 08

TOP_TB := tb_fp32mul_stream

# Compile order matters (packages first)
SRC := \
	src/fp32mul_cfg_pkg.vhd \
	src/fp32mul_moore.vhd \
	src/fp32mul_stream.vhd \
	tb/tb_fp32mul_stream.vhd

all: run

analyze:
	$(GHDL) -a --std=$(STD) $(SRC)

elab: analyze
	$(GHDL) -e --std=$(STD) $(TOP_TB)

TB_MUL_IMPL ?= 0

run: elab
	$(GHDL) -r --std=$(STD) $(TOP_TB) -gMUL_IMPL_G=$(TB_MUL_IMPL) --assert-level=error --vcd=wave.vcd

run_all: elab
	@echo "== COMB =="
	$(GHDL) -r --std=$(STD) $(TOP_TB) -gMUL_IMPL_G=0 --assert-level=error --vcd=wave_comb.vcd
	@echo "== ITER =="
	$(GHDL) -r --std=$(STD) $(TOP_TB) -gMUL_IMPL_G=1 --assert-level=error --vcd=wave_iter.vcd

wave: run
	@echo "Open wave.vcd in GTKWave (gtkwave wave.vcd)"

clean:
	rm -f *.o *.cf $(TOP_TB) wave.vcd wave_comb.vcd wave_iter.vcd

.PHONY: all analyze elab run run_all wave clean
