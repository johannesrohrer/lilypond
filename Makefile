include Variables.make 

.SUFFIXES:
.SUFFIXES: .cc .o .hh .y .l .pod .txt .1 .dep


$(exe): $(obs)
	$(STRIPDEBUG) $(STABLEOBS)
	$(LINKER) -o $@ $^ $(LOADLIBES)


.PHONY: clean docxx

clean:
	rm -f $(exe) $(DOCDIR)/* core $(obs) $(ALLDEPS)
	for SUBDIR in $(SUBDIRS); \
	do \
		$(MAKE) SUBDIR=$$SUBDIR -C $$SUBDIR clean;\
	done

distclean: clean
	rm -f  version.hh $(gencc) .GENERATE *~ $(ALLDEPS)

all: kompijl doc

win32: # value of $(OSTYPE) on windhoos; "make $OSTYPE" if u use bash :-)
	$(MAKE) -C . CXX=g++ 

doc:
	$(MAKE) -C Documentation doc

# doc++ documentation of classes
docxx: $(progdocs)	
	doc++ -kp -d $(DOCDIR) $^

$(OBJECTDIR)/%.o: $(CCDIR)/%.cc
	$(DODEP)\
	$(CXX) -c $(CXXFLAGS) $(OUTPUT_OPTION) 


include $(DEPDIR)/*.dep

$(OBJECTDIR)/version.o: $(obs) $(HEADERDIR)/version.hh

include Generate.make

dist:
	-mkdir $(DDIR)
	ln $(DFILES) $(DDIR)/
	for SUBDIR in $(SUBDIRS); \
	do	mkdir $(DDIR)/$$SUBDIR; \
		$(MAKE) SUBDIR=$$SUBDIR -C $$SUBDIR dist;\
	done
	tar cfz $(DNAME).tar.gz $(DNAME)/
	rm -rf $(DDIR)/

TAGS:
	$(MAKE) -C $(HEADERDIR) TAGS
	$(MAKE) -C $(CCDIR) TAGS
