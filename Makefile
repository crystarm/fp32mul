GHDL ?= ghdl
STD  ?= 08

TOP_TB_STREAM := tb_fp32mul_stream
TOP_TB_CORE   := tb_fp32mul

COMMON_SRC := \
	src/fp32mul_cfg_pkg.vhd \
	src/fp32mul_moore.vhd

STREAM_SRC := \
	$(COMMON_SRC) \
	src/fp32mul_stream.vhd \
	tb/tb_fp32mul_stream.vhd

CORE_SRC := \
	$(COMMON_SRC) \
	tb/tb_fp32mul.vhd

all: run

analyze:
	$(GHDL) -a --std=$(STD) $(STREAM_SRC)

elab: analyze
	$(GHDL) -e --std=$(STD) $(TOP_TB_STREAM)

analyze_core:
	$(GHDL) -a --std=$(STD) $(CORE_SRC)

elab_core: analyze_core
	$(GHDL) -e --std=$(STD) $(TOP_TB_CORE)

TB_MUL_IMPL ?= 0

run: elab
	$(GHDL) -r --std=$(STD) $(TOP_TB_STREAM) -gMUL_IMPL_G=$(TB_MUL_IMPL) --assert-level=error --vcd=wave.vcd

run_core: elab_core
	$(GHDL) -r --std=$(STD) $(TOP_TB_CORE) --assert-level=error --vcd=wave_core.vcd

run_all: elab
	@echo "== COMB =="
	$(GHDL) -r --std=$(STD) $(TOP_TB_STREAM) -gMUL_IMPL_G=0 --assert-level=error --vcd=wave_comb.vcd
	@echo "== ITER =="
	$(GHDL) -r --std=$(STD) $(TOP_TB_STREAM) -gMUL_IMPL_G=1 --assert-level=error --vcd=wave_iter.vcd

run_all_tests: run_all run_core

wave: run
	@echo "Open wave.vcd in GTKWave (gtkwave wave.vcd)"

clean:
	rm -f *.o *.cf $(TOP_TB_STREAM) $(TOP_TB_CORE) wave.vcd wave_comb.vcd wave_iter.vcd wave_core.vcd

.PHONY: all analyze elab analyze_core elab_core run run_core run_all run_all_tests wave clean
