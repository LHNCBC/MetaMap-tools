include ../Makefile.include

SICSTUSARGS=
PREFIX='$$SP_APP_DIR'
APPNAME=mm_variants
SAVED_STATE=$(APPNAME).sav
BINEXEC=$(APPNAME).BINARY.$(ARCH)

all : $(BINEXEC) 

$(SAVED_STATE) : $(SPRUNTIME) $(SOFILES)
	sicstus $(SICSTUSARGS) --goal "save_program('$(SAVED_STATE)'), halt."

$(BINEXEC) : $(SAVED_STATE)
	spld -vv $(CONF) --movable $(PREFIX)/$(SAVED_STATE) --respath=$(RESPATH) --output=$(BINEXEC) $(LINK_FILES) $(LDFLAGS)

clean :
	rm -f $(BINEXEC) $(SAVED_STATE)

