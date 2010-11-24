# -*- Makefile -*-

VPATH	= $(BINDIR)
exec	= $(BINDIR)/hotspot
src	= maxima_tools.f90 HotSpots.f90
obj 	= maxima_tools.o HotSpots.o

# %.o: %.F90
# 	case $(OS) in \
# 	AIX ) \
# 	ln -s -f $< $*.F; \
# 	$(FC) $(FFLAGS) -c $*.F -o $@; \
# 	rm $*.F;; \
# 	OSF ) \
# 	ln -s -f $< $*.f90; \
# 	$(FC) $(FFLAGS) -cpp -c $(basename $<).f90 -o $@;; \
# 	* ) \
# 	$(FC) $(FFLAGS) -c $< -o $@;; \
# 	esac
# %.o: %.f90
# 	case $(OS) in \
# 	AIX ) \
# 	ln -s -f $< $*.f; \
# 	$(FC) $(FFLAGS) -c $*.f -o $@; \
# 	rm $*.f;; \
# 	* ) \
# 	$(FC) $(FFLAGS) -c $< -o $@;; \
# 	esac
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

default: $(exec)

$(exec): $(obj) $(LIBDIR)/libhealpix.a
	$(FC) $(FFLAGS) -o $(exec) $(obj) $(LDFLAGS)

# spell out dependencies to allow parallel compilation (make -j)
HotSpots.o: maxima_tools.o

