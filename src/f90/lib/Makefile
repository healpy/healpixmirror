# -*- Makefile -*-

# VPATH	= $(HEALPIX)/lib

modsrc	= gifmod.f90
modobj	= gifmod.o

gifsrc	= gifout.c gifarr.c gifdim.c gifstr.c chopst.c
gifobj	= gifout.o gifarr.o gifdim.o gifstr.o chopst.o

libsrc	= gd.c gdfontt.c gdfonts.c gdfontmb.c gdfontl.c gdfontg.c
libobj	= gd.o gdfontt.o gdfonts.o gdfontmb.o gdfontl.o gdfontg.o
libhdr	= gd.h gdfontt.h gdfonts.h gdfontmb.h gdfontl.h gdfontg.h

allobj	= $(libobj) $(gifobj) $(modobj)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

default: libhpxgif.a

libhpxgif.a: $(allobj)
	$(AR) libhpxgif.a $(allobj)
	@cp -p libhpxgif.a $(LIBDIR)/
	@cp -p libhpxgif.a $(LIBDIR)/libgif.a
	@for p in *.$(MOD); do \
	cp $$p $(INCDIR)/; \
	done
