# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/anafast
src	= anafast.f90
obj 	= anafast.o
inc     = ana_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
