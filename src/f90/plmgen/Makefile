# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/plmgen
src	= plmgen.f90
obj 	= plmgen.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
