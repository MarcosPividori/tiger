CP=cp
BUILDDIR=build
SRCDIR=src

SRCS:=$(wildcard $(SRCDIR)/*)
SRCSBUILD:=$(SRCS:$(SRCDIR)/%=$(BUILDDIR)/%)

.PHONY: all clean

all: $(SRCSBUILD)
	$(MAKE) -C $(BUILDDIR)

clean:
	$(RM) -rf $(BUILDDIR)

$(BUILDDIR):
	@mkdir $(BUILDDIR)

$(BUILDDIR)/%: $(SRCDIR)/% | $(BUILDDIR)
	@$(CP) $< $@
