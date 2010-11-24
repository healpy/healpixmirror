# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/map2gif
src	= euler_matrix_new.f90 map2gif.f90
obj 	= euler_matrix_new.o   map2gif.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a $(LIBDIR)/libhpxgif.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
