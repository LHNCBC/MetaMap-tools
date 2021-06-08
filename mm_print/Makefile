include ../Makefile.include

SICSTUSARGS=
PREFIX='$$SP_APP_DIR'
APPNAME=mm_print
SAVED_STATE=$(APPNAME).sav
BINEXEC=$(APPNAME).BINARY.$(ARCH)

all : $(BINEXEC) 

$(SAVED_STATE) : 
	$(PROLOG) $(SICSTUSARGS) --goal "save_program('$(SAVED_STATE)'), halt."

$(BINEXEC) : $(SAVED_STATE) $(SOFILES)
	$(SPLD) -vv \
	--respath=$(RESPATH) \
	--output=$(BINEXEC) \
	--static \
	--main=restore \
	--resources=$(SAVED_STATE)=/$(SAVED_STATE) $(LINK_FILES) $(LDFLAGS)

clean :
	rm -f $(BINEXEC) $(SAVED_STATE)

