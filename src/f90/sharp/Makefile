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
###LIB:=$(OD)/libsharp_healpix_f$(LIBSUFFIX)
LIB:=$(LIBDIR)/libsharp_healpix_f$(LIBSUFFIX)


%_mkdir:
	@if [ ! -d $* ]; then $(MKDIR) $* ; fi 

$(OD)/%.o: $(SD)/%.c $(inc) $(OD)_mkdir
	$(CC) $(CFLAGS) -I c_utils -I libfftpack -I libsharp -I libsharp_healpix_f -c $< -o $@

default: $(LIB)

$(LIB): $(OBJ)
	$(AR)  $(LIB) $(OBJ)

# $(LIB): $(OBJ)
# 	$(AR)  $(LIB) $(OBJ)
# 	@cp -p $(LIB) $(LIBDIR)/
