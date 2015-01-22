VHDL_TARGETS = vhdl lt24top

vhdl_TOP = CLaSHMain.hs
lt24top_TOP = UnitTest/LT24/LT24/LT24Top.hs

define VHDL_template
$(1): $$($(1)_TOP)
	clash --vhdl $$< 2>&1 | tee make.log
endef

$(foreach tgt,$(VHDL_TARGETS),$(eval $(call VHDL_template,$(tgt))))

.PHONY: clean $(VHDL_TARGETS)

clean:
	find . -name '*.o' -type f -delete
	find . -name '*.hi' -type f -delete
	rm -rf vhdl/
	rm -f make.log
