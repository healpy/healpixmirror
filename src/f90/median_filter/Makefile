# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/median_filter
src	= median_filter.f90
obj 	= median_filter.o
inc     = med_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
