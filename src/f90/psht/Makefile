# -*- Makefile -*-

libsrc	= libpsht_all.c
libobj	= libpsht_all.o

allobj	= $(libobj)

%.o: %.c
	$(CC) $(CFLAGS) -I c_utils -I libfftpack -I libpsht -I libpsht_healpix_f -c $< -o $@

default: libpsht_healpix_f.a

libpsht_healpix_f.a: $(libobj)
	$(AR) libpsht_healpix_f.a $(libobj)
	@cp -p libpsht_healpix_f.a $(LIBDIR)/
