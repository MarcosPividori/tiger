CP=cp
BUILDDIR=build
SRCDIR=src
TESTDIR=test

SRCS:=$(wildcard $(SRCDIR)/*)
SRCSBUILD:=$(SRCS:$(SRCDIR)/%=$(BUILDDIR)/%)

.PHONY: all clean test

all: $(SRCSBUILD)
	$(MAKE) -C $(BUILDDIR)

clean:
	$(RM) -rf $(BUILDDIR)

test: all
	$(MAKE) -C $(TESTDIR)

$(BUILDDIR):
	@mkdir $(BUILDDIR)

$(BUILDDIR)/%: $(SRCDIR)/% | $(BUILDDIR)
	@$(CP) $< $@
