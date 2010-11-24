# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/ud_grade
src	= ud_grade.f90
obj 	= ud_grade.o
inc     = udg_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
