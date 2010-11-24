# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/alteralm
src	= alteralm.f90
obj 	= alteralm.o
inc	= alt_sub_inc.f90

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
