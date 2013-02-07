.PHONY : all clean bin-clean default dist exe help html lib TAGS\
	 po doc doc-stage-1 WWW-1 WWW-2 WWW-post local-WWW-1 local-WWW-2\
	 log-clean

all:	 default
	$(LOOP)

man:
	$(LOOP)

clean: local-clean
	-rm -rf $(outdir)
	$(LOOP)

log-clean:
	find . -name "*.log" -delete

ifeq (,$(findstring metafont,$(STEPMAKE_TEMPLATES)))
bin-clean: local-bin-clean
	-rm -rf $(outdir)
	$(LOOP)
else
bin-clean:
endif

local-bin-clean: local-clean

ifneq ($(strip $(depth)),.)
dist:
	$(MAKE) -C $(depth) dist
endif

generic-help:
	@echo "Makefile for $(PACKAGE_NAME) $(TOPLEVEL_VERSION)"
	@echo "Usage: make ["VARIABLE=value"]... [TARGET]"
	@echo
	@echo "Targets specific to current directory:"

help: generic-help local-help
	@echo "Generic targets that recurse into subdirectories*:"
	@echo "  all          update everything except documentation with images"
	@echo "  clean        remove all generated stuff in $(outdir)"
	@echo "  bin-clean    same as clean, except that mf/out is preserved"
	@echo "  doc          update documentation with images in directory \`out-www'"
	@echo "  doc-stage-1  update only PDF and Info documentation in directory \`out-www'"
	@echo "  doc-clean    clean \`out-www' directory"
	@echo "  install      install programs and data (prefix=$(prefix))"
	@echo "  uninstall    uninstall programs and data"
	@echo "  test         build regression tests for the program and scripts"
	@echo
	@echo "  *Note: Prepend \`local-' (eg. \`local-clean') to restrict"
	@echo "         any of the above commands to the current directory."
	@echo
	@echo "Other generic targets:"
	@echo "  default      same as the empty target"
	@echo "  exe          update all executables"
	@echo "  help         this help"
	@echo "  lib          update all libraries"
	@echo "  log-clean    remove .log files"
	@echo "  TAGS         generate tagfiles"
	@echo
	@echo "\`make' may be invoked from any subdirectory that contains a GNUmakefile."

local-help:

local-dist: $(OUT_DIST_FILES)
	case "$(OUT_DIST_FILES)x" in x) ;; \
	     *) mkdir -p $(distdir)/$(localdir)/$(outdir) && \
	        $(LN) $(OUT_DIST_FILES) $(distdir)/$(localdir)/$(outdir);; \
	esac
	$(foreach i, $(SUBDIRS), $(MAKE) top-src-dir=$(top-src-dir) distdir=$(distdir) localdir=$(localdir)/$(notdir $(i)) -C $(i) local-dist &&) true



html: $(HTML_FILES)

TAGS:
	$(LOOP)
	$(MAKE) local-tags

DEEPER_TAGS_FILES = $(shell find $(pwd) -mindepth 2 -name 'TAGS')
local-tags:
	-if [ -n "$(TAGS_HEADERS)$(TAGS_SOURCES)$(DEEPER_TAGS_FILES)" ]; then \
		etags $(ETAGS_FLAGS) $(DEEPER_TAGS_FILES:%=--include=%) \
			$(TAGS_SOURCES) $(TAGS_HEADERS) $(ERROR_LOG) ; \
		ctags $(CTAGS_FLAGS) $(TAGS_SOURCES) $(TAGS_HEADERS) \
			$(ERROR_LOG) ; \
	fi

# Don't use $(buildscript-dir)/make-version, because it is not known whether
# build process has visited scripts/build
$(outdir)/version.hh: $(depth)/VERSION $(config_make) $(top-src-dir)/scripts/build/make-version.py
	$(PYTHON) $(top-src-dir)/scripts/build/make-version.py $< > $@

$(outdir)/config.hh: $(config_h)
	cp -p $< $@

configure: configure.ac aclocal.m4
	NOCONFIGURE=yes $(src-depth)/autogen.sh
	chmod +x configure

local-clean:

local-distclean:

install-strip:
	$(MAKE) INSTALLPY="$(INSTALLPY) -s" install

ifeq ($(strip $(depth)),.)
final-install:
else
final-install:
	$(LOOP)

install: local-install
	$(LOOP)
endif

local-install:

uninstall: local-uninstall
	$(LOOP)

local-uninstall:

installextradoc:
	-$(INSTALLPY) -d $(DESTDIR)$(prefix)/doc/$(package)
	cp -r $(EXTRA_DOC_FILES) $(prefix)/doc/$(package)

-include $(outdir)/dummy.dep $(wildcard $(outdir)/*.dep)

$(outdir)/dummy.dep:
	-mkdir -p $(outdir)
	touch $(outdir)/dummy.dep
	echo '*' > $(outdir)/.gitignore

check: local-check
	$(LOOP)

local-check:

# ugh.  ugh ugh ugh
$(config_make): $(top-src-dir)/configure
	@echo "************************************************************"
	@echo "configure changed! You should probably reconfigure manually."
	@echo "************************************************************"
	(cd $(top-build-dir); ./config.status)
	touch $@		# do something for multiple simultaneous configs.


#### Documentation (website and tarball)

# documentation is built in two stages,
# plus WWW-post (only at toplevel)
# see INSTALL for more information.

ifeq ($(out),www)
local-WWW-1:
local-WWW-2:
WWW-post:

WWW-1: local-WWW-1
	$(LOOP)

WWW-2: local-WWW-2
	$(LOOP)
endif

doc: doc-stage-1
	$(MAKE) out=www WWW-2
	$(MAKE) out=www WWW-post

local-doc:
	$(MAKE) out=www local-WWW-1
	$(MAKE) out=www local-WWW-2
	$(MAKE) out=www WWW-post

doc-stage-1:
	$(MAKE) -C $(top-build-dir)/Documentation/po out=www messages
	$(MAKE) -C $(depth)/scripts/build out=
	$(MAKE) out=www WWW-1

doc-clean:
	$(MAKE) out=www clean
