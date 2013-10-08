# -*- Makefile -*-

fac	= lib
inc     = 
VPATH	= $(BINDIR)
#exec	= $(BINDIR)/$(fac)
#src	= $(fac).f90
modobj	= gifmod.o
gifobj	= gifout.o gifarr.o gifdim.o gifstr.o chopst.o
libobj	= gd.o gdfontt.o gdfonts.o gdfontmb.o gdfontl.o gdfontg.o
obj 	= $(libobj) $(gifobj) $(modobj)

MKDIR   = mkdir -p
OD :=$(BUILDDIR)/$(fac)
SD :=$(shell pwd)
OBJ:=$(obj:%=$(OD)/%)
###LIB:=$(OD)/libhpxgif$(LIBSUFFIX)
LIB:=$(LIBDIR)/libhpxgif$(LIBSUFFIX)


%_mkdir:
	@if [ ! -d $* ]; then $(MKDIR) $* ; fi 


$(OD)/%.o: $(SD)/%.f90 $(inc) $(OD)_mkdir
	$(FC) $(FFLAGS) -c $< -o $@ $(MODDIR)$(OD)

$(OD)/%.o: $(SD)/%.c $(inc) $(OD)_mkdir
	$(CC) $(CFLAGS) -c $< -o $@


default: $(LIB)

$(LIB): $(OBJ)
	$(AR)  $(LIB) $(OBJ)
	@cp -p $(LIB) $(LIBDIR)/libgif$(LIBSUFFIX)
	@for p in $(OD)/*.$(MOD); do \
	cp $$p $(INCDIR)/; \
	done

# $(LIB): $(OBJ)
# 	$(AR)  $(LIB) $(OBJ)
# 	@cp -p $(LIB) $(LIBDIR)/
# 	@cp -p $(LIB) $(LIBDIR)/libgif$(LIBSUFFIX)
# 	@for p in $(OD)/*.$(MOD); do \
# 	cp $$p $(INCDIR)/; \
# 	done

