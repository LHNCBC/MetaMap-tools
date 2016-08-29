include ../Makefile.include

#
# make SKR=\${ROOT}/specialist/SKR \
#    MACHINE_TYPE=-m64  \
#    ARCHDIR=x86_64-linux-glibc2.12  \
#    BERKELEY=\${BERKELEY_DB_64bit}/db-4.8.24  \
#    SICSTUS=/nfsvol/crfiler-ind/II_Research/SICStus/sp-\$(SICSTUS_VERSION)-x86_64-linux-glibc2.12

SICSTUSARGS=
PREFIX='$$SP_APP_DIR'
APPNAME=filter_mrconso
SAVED_STATE=$(APPNAME).sav
BINEXEC=$(APPNAME).BINARY.$(ARCH)

all : $(BINEXEC) $(SAVED_STATE)

$(SAVED_STATE) : $(PROLOGSHOBJS)
	$(PROLOG) $(SICSTUSARGS) --goal "save_program('$(SAVED_STATE)'), halt."

$(BINEXEC) : $(SAVED_STATE)
	$(SPLD) -vv \
	--respath=$(RESPATH) \
	--output=$(BINEXEC) \
	--static \
	--main=restore \
	--resources=$(SAVED_STATE)=/$(SAVED_STATE) $(LINK_FILES) $(LDFLAGS)

# clean :
#	rm -f $(BINEXEC) $(SAVED_STATE)

