# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/process_mask
src	= process_mask.F90
obj 	= process_mask.o
# inc     = 

%.o: %.f90 $(inc)
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.F90
	@echo compiling $*.F90
	@case $(OS) in \
	Darwin ) \
	ln -s -f $< $*.junk.F90; \
	$(FC) $(FFLAGS)  $(ADDUS) $(PPFLAGS)  -c $*.junk.F90 -o $@; \
	rm $*.junk.F90;; \
	OSF ) \
	ln -s -f $< $*.f90; \
	$(FC) $(FFLAGS)  $(ADDUS) -cpp        -c $(basename $<).f90 -o $@;; \
	* ) \
	$(FC) $(FFLAGS)  $(ADDUS) $(PPFLAGS)  -c $< -o $@;; \
	esac

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)
