# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/synfast
src	= synfast.f90
obj 	= synfast.o
inc     = syn_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
