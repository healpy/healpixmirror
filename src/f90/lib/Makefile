# -*- Makefile -*-

fac	= lib
inc     = 
VPATH	= $(BINDIR)
exec	= $(BINDIR)/$(fac)
#src	= $(fac).f90
modobj	= gifmod.o
gifobj	= gifout.o gifarr.o gifdim.o gifstr.o chopst.o
libobj	= gd.o gdfontt.o gdfonts.o gdfontmb.o gdfontl.o gdfontg.o
obj 	= $(libobj) $(gifobj) $(modobj)

MKDIR   = mkdir -p
OD :=$(BUILDDIR)/$(fac)
SD :=$(shell pwd)
OBJ:=$(obj:%=$(OD)/%)


%_mkdir:
	@if [ ! -d $* ]; then $(MKDIR) $* ; fi 


$(OD)/%.o: $(SD)/%.f90 $(inc) $(OD)_mkdir
	$(FC) $(FFLAGS) -c $< -o $@ $(MODDIR)$(OD)

$(OD)/%.o: $(SD)/%.c $(inc) $(OD)_mkdir
	$(CC) $(CFLAGS) -c $< -o $@


default: $(OD)/libhpxgif.a

$(OD)/libhpxgif.a: $(OBJ)
	$(AR)  $(OD)/libhpxgif.a $(OBJ)
	@cp -p $(OD)/libhpxgif.a $(LIBDIR)/
	@cp -p $(OD)/libhpxgif.a $(LIBDIR)/libgif.a
	@for p in $(OD)/*.$(MOD); do \
	cp $$p $(INCDIR)/; \
	done

