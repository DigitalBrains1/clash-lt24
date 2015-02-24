VHDL_TARGETS = vhdl uartif uartif_inited lt24top hallotx hallotx_fifo echo_swap_case

vhdl_TOP = CLaSHMain.hs
uartif_TOP = UnitTest/LT24/UARTInterface/IntfBare.hs
uartif_inited_TOP = UnitTest/LT24/UARTInterface/IntfInited.hs
lt24top_TOP = UnitTest/LT24/LT24/LT24Top.hs
hallotx_TOP = UnitTest/Toolbox/Serial/HalloTransmitter.hs
hallotx_fifo_TOP = UnitTest/Toolbox/Serial/HalloTransmitterFIFO.hs
echo_swap_case_TOP = UnitTest/Toolbox/Serial/EchoSwapCase.hs

define VHDL_template
$(1): $$($(1)_TOP)
	clash --vhdl $$< 2>&1 | tee make.log
endef

$(foreach tgt,$(VHDL_TARGETS),$(eval $(call VHDL_template,$(tgt))))

check:
	for t in $(foreach tgt, $(VHDL_TARGETS), $($(tgt)_TOP)) Main.hs; do \
		clash $$t 2>&1 || exit $?; \
	done | tee make.log

main:
	clash Main.hs 2>&1 | tee make.log

.PHONY: clean check $(VHDL_TARGETS) main

clean:
	find . -name '*.o' -type f -delete
	find . -name '*.hi' -type f -delete
	rm -rf vhdl/
	rm -f make.log
