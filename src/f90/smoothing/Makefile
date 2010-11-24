# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/smoothing
src	= smoothing.f90
obj 	= smoothing.o
inc 	= smo_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
