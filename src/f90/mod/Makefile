# -*- Makefile -*-


incalm  = alm_map_ss_inc.F90 alm_map_dd_inc.F90

incpix = convert_inplace_1d_inc.f90 \
	convert_inplace_nd_inc.f90 \
	convert_nest2ring_nd_inc.f90 \
	convert_nest2ring_1d_inc.f90 \
	convert_ring2nest_nd_inc.f90 \
	convert_ring2nest_1d_inc.f90 \
	remove_dipole_inc.f90 \
	pixel_routines.F90

incfits = fits_s_inc.f90 fits_d_inc.f90

incudgrade = udgrade_s_inc.f90 udgrade_d_inc.f90

incnumrec = source_gen_sort_inc.f90

obj0  = healpix_types.o cgetEnvironment.o indmed.o utilities.o 
obj1  = bit_manipulation.o extension.o long_intrinsic.o obsolete.o ran_tools_dist.o rngmod.o
obj2  = healpix_fft.o misc_utils.o
obj3  = head_fits.o num_rec.o statistics.o paramfile_io.o 
obj4  = pix_tools.o 
obj5  = coord_v_convert.o fitstools.o udgrade_nr.o 
obj6  = mask_tools.o alm_tools.o
obj7  = healpix_modules.o

obj   = $(obj0) $(obj1) $(obj2) $(obj3) $(obj4) $(obj5) $(obj6) $(obj7)
objsharp = ../sharp/libsharp_all.o

default: libhealpix.a 

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

%.o: %.F90
	@echo compiling $*.F90
	@case $(OS) in \
	Darwin ) \
	ln -s -f $< $*.junk.F90; \
	$(FC) $(FFLAGS) $(ADDUS) $(PPFLAGS) -c $*.junk.F90 -o $@; \
	rm $*.junk.F90;; \
	OSF ) \
	ln -s -f $< $*.f90; \
	$(FC) $(FFLAGS) $(ADDUS) -cpp -c $(basename $<).f90 -o $@;; \
	* ) \
	$(FC) $(FFLAGS) $(ADDUS) $(PPFLAGS) -c $< -o $@;; \
	esac

long_intrinsic.o: long_intrinsic.F90
	@echo compiling $*.F90
	@case $(OS) in \
	Darwin ) \
	ln -s -f $< $*.junk.F90; \
	$(FC) $(FFLAGS) $(ADDUS) $(PPFLAGS) $(FI8FLAG) -c $*.junk.F90 -o $@; \
	rm $*.junk.F90;; \
	OSF ) \
	ln -s -f $< $*.f90; \
	$(FC) $(FFLAGS) $(ADDUS) -cpp $(FI8FLAG) -c $(basename $<).f90 -o $@;; \
	* ) \
	$(FC) $(FFLAGS) $(ADDUS) $(PPFLAGS) $(FI8FLAG) -c $< -o $@;; \
	esac


libhealpix.a: $(obj) $(objsharp) $(incpix) $(incalm) $(incfits) $(incudgrade)
	$(AR) libhealpix.a $(obj) $(objsharp)
	@cp -p libhealpix.a $(LIBDIR)/
	@for p in *.$(MOD); do \
	cp $$p $(INCDIR)/; \
	done

alm_map_ss_inc.F90: alm_map_template.F90 gen_alm_code
	@./gen_alm_code

alm_map_dd_inc.F90: alm_map_template.F90 gen_alm_code alm_map_ss_inc.F90
	@./gen_alm_code

fits_s_inc.f90: fits_template.f90 gen_fits_code
	@./gen_fits_code

fits_d_inc.f90: fits_template.f90 gen_fits_code fits_s_inc.f90
	@./gen_fits_code

udgrade_s_inc.f90: udgrade_template.f90 gen_udgrade_code
	@./gen_udgrade_code

udgrade_d_inc.f90: udgrade_template.f90 gen_udgrade_code udgrade_s_inc.f90
	@./gen_udgrade_code

pix_tools.o:pix_tools.F90  $(incpix)

alm_tools.o:alm_tools.F90  $(incalm)

fitstools.o: fitstools.F90 $(incfits)

udgrade_nr.o: udgrade_nr.f90 $(incudgrade)

num_rec.o: num_rec.F90 $(incnumrec)

mask_tools.o: mask_tools.F90


#obj1
bit_manipulation.o:  $(obj0)
extension.o:         $(obj0)
long_intrinsic.o:    $(obj0)
obsolete.o:          $(obj0)
ran_tools_dist.o:    $(obj0)
rngmod.o:            $(obj0)

#obj2
healpix_fft.o :  $(obj1)
misc_utils.o:    $(obj1)

#obj3
head_fits.o :     $(obj2)
num_rec.o :       $(obj2)
paramfile_io.o :  $(obj2)
statistics.o :    $(obj2)

#obj4
pix_tools.o:  $(obj3)

#obj5
fitstools.o :        $(obj4)
coord_v_convert.o :  $(obj4)
udgrade_nr.o :       $(obj4)

#obj6
alm_tools.o: $(obj5)
mask_tools.o: $(obj5)

#obj7
healpix_modules.o: $(obj6)

