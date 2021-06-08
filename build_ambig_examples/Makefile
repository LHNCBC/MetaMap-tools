include ../Makefile.include

SICSTUSARGS=
PREFIX='$$SP_APP_DIR'
APPNAME=build_ambig_examples
SAVED_STATE=$(APPNAME).sav
BINEXEC=$(APPNAME).BINARY.$(ARCH)

all : $(BINEXEC) 

$(SAVED_STATE) : $(SPRUNTIME) $(SOFILES)
	sicstus $(SICSTUSARGS) --goal "save_program('$(SAVED_STATE)'), halt."

$(BINEXEC) : $(SAVED_STATE)
	$(SPLD) -vv \
	--respath=$(RESPATH) \
	--output=$(BINEXEC) \
	--static \
	--main=restore \
	--resources=$(SAVED_STATE)=/$(SAVED_STATE) $(LINK_FILES) $(LDFLAGS)

clean :
	rm -f $(BINEXEC) $(SAVED_STATE)

