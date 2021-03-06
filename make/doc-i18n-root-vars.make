# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(top-src-dir)/python/langdefs.py)

TELY_FILES := $(call src-wildcard,*.tely)
TEXI_FILES := $(call src-wildcard,*.texi)
MASTER_TEXI_FILES := $(TEXI_FILES) $(TELY_FILES:%.tely=$(outdir)/%.texi)

TEXINFO_MANUALS =\
 $(TELY_FILES:%.tely=%)\
 $(TEXI_FILES:%.texi=%)
OUT_TEXINFO_MANUALS = $(TEXINFO_MANUALS:%=$(outdir)/%.texi)

TOPDIR_HTML_MANUALS = 
SPLIT_HTML_MANUALS = $(foreach manual, $(TEXINFO_MANUALS),\
 $(if $(findstring $(manual), $(UNSPLIT_HTML_MANUALS)),,$(manual)))
NOT_TOPDIR_HTML_MANUALS = $(foreach manual, $(SPLIT_HTML_MANUALS),\
 $(if $(findstring $(manual), $(TOPDIR_HTML_MANUALS)),,$(manual)))

OUT_HTML_FILES += $(UNSPLIT_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).html) \
 $(TOPDIR_HTML_MANUALS:%=$(outdir)/index.$(ISOLANG).html)
BIG_PAGE_HTML_FILES := $(SPLIT_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html)
DEEP_HTML_FILES := $(NOT_TOPDIR_HTML_MANUALS:%=$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html)
PDF_FILES := $(TELY_FILES:%.tely=$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation/$(ISOLANG)/included \
  -I $(top-src-dir)/Documentation \
  -I $(top-build-dir)/Documentation/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

WEB_MANUALS=web

###########
ifneq ($(ISOLANG),)
TEXI2HTML_LANG = --lang=$(ISOLANG)
endif

$(XREF_MAPS_DIR)/web.$(ISOLANG).xref-map:\
	XREF_MAP_FLAGS += --split=node

TEXI2HTML_INIT = --init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init

TEXI2HTML_SPLIT = --prefix=index --split=section

TEXI2HTML_INCLUDES += --I=. --I=$(src-dir) --I=$(outdir) $(DOCUMENTATION_INCLUDES) --I=$(XREF_MAPS_DIR)
TEXI2HTML_FLAGS += $(TEXI2HTML_INCLUDES) $(TEXI2HTML_LANG) $(TEXI2HTML_INIT) 
TEXI2HTML = TOP_SRC_DIR=$(top-src-dir) PERL_UNICODE=SD $(TEXI2HTML_PROGRAM)
###########

TEXI2PDF_FLAGS +=  $(DOCUMENTATION_INCLUDES)
TEXI2PDF_FLAGS += -I $(LYS_OUTPUT_DIR)

DOCUMENTATION_LOCALE_TARGET = $(outdir)/doc-po
TRANSLATION_LILY_IMAGES = $(outdir)/translation-lily-images
