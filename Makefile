build_dir := build
intralisp_files := $(wildcard *.intralisp.vhd)
intralisp_design_files := $(foreach file, $(intralisp_files), $(shell echo $(file) | grep -v _TB))
intralisp_simulation_files := $(foreach file, $(intralisp_files), $(shell echo $(file) | grep _TB))
object_files := $(addprefix $(build_dir)/, $(intralisp_files:.intralisp.vhd=.o))
design_vhd_files := $(addprefix $(build_dir)/design/, $(intralisp_design_files:.intralisp.vhd=.vhd))
simulation_vhd_files := $(addprefix $(build_dir)/simulation/, $(intralisp_simulation_files:.intralisp.vhd=.vhd))
entity_names = $(shell ghdl files $(wildcard $(build_dir)/simulation/*.vhd) $(wildcard $(build_dir)/design/*.vhd) | grep entity | sort -u | cut -f2 -d' ')
entity_files = $(addprefix $(build_dir)/, $(entity_names))
wave_files = $(addsuffix .vcd, $(entity_files:$(build_dir)=$(build_dir)/waves/))
remote_project_dir := /users/ugrad/uzoukwuc/shelinksveevathough/registerfile

what:
	echo $(design_vhd_files)

all: build intralisp analyse elaborate visualise
	@echo Your CPU is ready! 🤖

elaborate: $(entity_files)

$(entity_files): analyse
	ghdl -e --workdir=$(build_dir) -o $@ $$(basename $@)

analyse: build intralisp $(object_files)

intralisp: build $(design_vhd_files) $(simulation_vhd_files)
	intralisp project.xpr.intralisp > $(build_dir)/registerfile.xpr
	intralisp ripple.intralisp.html > $(build_dir)/ripple.html

$(build_dir)/%.o: $(build_dir)/design/%.vhd $(build_dir)/simulation/%.vhd
	ghdl -a --workdir=$(build_dir) $<

$(build_dir)/design/%.vhd $(build_dir)/simulation/%.vhd: %.intralisp.vhd
	intralisp $< > $@

visualise: $(wave_files)

$(wave_files): build elaborate
	cd $(build_dir); wave=$$(basename $@); ghdl run $${wave%.vcd} --vcd=waves/$$wave

build: $(build_dir)/design $(build_dir)/simulation $(build_dir)/waves

$(build_dir)/design $(build_dir)/simulation $(build_dir)/waves: 
	mkdir -p $@

clean:
	rm -fr $(build_dir)

sync: intralisp
	rsync --no-motd --checksum -azP --delete $(build_dir)/simulation/ 'm:$(remote_project_dir)/registerfile.srcs/sim_1/imports/simulation'
	rsync --no-motd --checksum -azP --delete $(build_dir)/design/ 'm:$(remote_project_dir)/registerfile.srcs/sources_1/imports/design'
	rsync --no-motd --checksum -azP --delete $(build_dir)/registerfile.xpr 'm:$(remote_project_dir)/registerfile.xpr'

.PHONY: $(entities) elaborate clean
