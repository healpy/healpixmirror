# -*- Makefile -*-

fac     = sharp
inc     = 
#VPATH	= $(BINDIR)
#exec	= $(BINDIR)/$(fac)
#src	= $(fac).f90
obj	= libsharp_all.o

MKDIR   = mkdir -p
OD :=$(BUILDDIR)/$(fac)
SD :=$(shell pwd)
OBJ:=$(obj:%=$(OD)/%)


%_mkdir:
	@if [ ! -d $* ]; then $(MKDIR) $* ; fi 

$(OD)/%.o: $(SD)/%.c $(inc) $(OD)_mkdir
	$(CC) $(CFLAGS) -I c_utils -I libfftpack -I libsharp -I libsharp_healpix_f -c $< -o $@

default: $(OD)/libsharp_healpix_f.a

$(OD)/libsharp_healpix_f.a: $(OBJ)
	$(AR)  $(OD)/libsharp_healpix_f.a $(OBJ)
	@cp -p $(OD)/libsharp_healpix_f.a $(LIBDIR)/
