!-----------------------------------------------------------------------------
!
!  Copyright (C) 1997-2010 Krzysztof M. Gorski, Eric Hivon,
!                          Benjamin D. Wandelt, Anthony J. Banday, 
!                          Matthias Bartelmann, Hans K. Eriksen, 
!                          Frode K. Hansen, Martin Reinecke
!
!
!  This file is part of HEALPix.
!
!  HEALPix is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!
!  HEALPix is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with HEALPix; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
!  For more information about HEALPix see http://healpix.jpl.nasa.gov
!
!-----------------------------------------------------------------------------
module pix_tools
!==================================================================
!    Various subroutines for converting angles and unit vectors
!    to pixel numbers in both the NESTED and RING schemes,
!    as well as looking for neighbours and making pixel queries
!
!    last update : Oct 4, 2002, EH, computation of pixel vertex in
!        pix2vec_ring and pix2vec_nest
!    2004-05-28, EH: edition of next_in_line_nest for nside=1
!                    addition of optional 'mask' in remove_dipole
!    2004-06-01      correction of bug at low phi in in_ring
!    2004-08-25      added multi-D convert_inplace_* and convert_nest2ring, convert_ring2nest
!    2004-10-07      added template_pixel_*, same_shape_pixels_*
!    2004-12-15      generic forms for convert_inplace (I,R,D with 1 or N dimensions), 
!                     remove_dipole (R,D)
!    2005-01-25      ensure backward compatibility of remove_dipole
!    2005-08-03      edit same_shape_pixels_nest to comply with GFORTRAN
!    2006-06-28      edit to neighbours_nest to correct error in Nside=1 case
!    2008-02-03:     addition of weights in remove dipole
!    2008-02-04:     check ordering in [1,2] in remove dipole ; check nest in [0,1] in query_*
!    2008-03-28:     edit ring_num and query_strip to improve inclusive query_strip
!                    bug fix on query_disc
!    2009-03-04:     starts supporting 64-bit npix
!              largest supported Nside is now 2^28 instead of 2^13
!             Note: the theoretical limit Nside=2^29 is not implemented to avoid
!               dealing with too many 64 bit integer variables that slow down execution
!==================================================================
  ! subroutine query_strip                          Done (To be Tested) depends on in_ring
  ! subroutine query_polygon                        Done (To be Tested) depends on isort
  ! subroutine query_triangle                       Done (To be Tested)
  ! subroutine getdisc_ring                         Done depends on query_disc
  ! subroutine query_disc                           Done (To be Tested)
  ! function ring_num                                     OK
  ! function ring2z                                 Done  
  ! subroutine in_ring                              Done (To be Tested) depends in next_in_line_nest
  ! subroutine intrs_intrv                                OK
  ! subroutine correct_ring_phi                           OK
  !                                         
  ! subroutine pix2ang_ring                          Done  
  ! subroutine pix2vec_ring                          Done  
  ! subroutine ang2pix_ring                          Done  
  ! subroutine vec2pix_ring                          Done  
  ! subroutine ang2pix_nest                          Done  
  ! subroutine vec2pix_nest                          Done  
  ! subroutine nest2ring                             Done  
  ! subroutine ring2nest                             Done  
  ! function npix2nside                              Done  
  !                                         
  ! subroutine warning_oldbounds                           OK
  ! subroutine remove_dipole_real                    Done depends on query_strip
  ! subroutine remove_dipole_double                  Done
  ! subroutine remove_dipole_real_old                Done
  ! subroutine remove_dipole_double_old              Done
  ! subroutine remove_dipole_real_v12                Done
  ! subroutine remove_dipole_double_v12              Done
  ! subroutine add_dipole_real                      Done  
  ! subroutine add_dipole_double                    Done  
  ! subroutine medfiltmap_S                         Done (To be tested)
  ! subroutine medfiltmap_D                         Done (To be tested)

  ! subroutine convert_nest2ring_int_1d             Done
  ! subroutine convert_nest2ring_int8_1d            Done
  ! subroutine convert_nest2ring_real_1d            Done
  ! subroutine convert_nest2ring_double_1d          Done      
  ! subroutine convert_ring2nest_int_1d             Done
  ! subroutine convert_ring2nest_int8_1d            Done
  ! subroutine convert_ring2nest_real_1d            Done
  ! subroutine convert_ring2nest_double_1d          Done
  ! subroutine convert_nest2ring_int_nd             Done
  ! subroutine convert_nest2ring_int8_nd            Done
  ! subroutine convert_nest2ring_real_nd            Done
  ! subroutine convert_nest2ring_double_nd          Done      
  ! subroutine convert_ring2nest_int_nd             Done
  ! subroutine convert_ring2nest_int8_nd            Done
  ! subroutine convert_ring2nest_real_nd            Done
  ! subroutine convert_ring2nest_double_nd          Done
  !
  ! subroutine convert_inplace_int_1d               Done (to be tested)
  ! subroutine convert_inplace_real_1d              Done (to be tested)
  ! subroutine convert_inplace_double_1d            Done (to be tested)
  ! subroutine convert_inplace_int_nd               Done (to be tested)
  ! subroutine convert_inplace_real_nd              Done (to be tested)
  ! subroutine convert_inplace_double_nd            Done (to be tested)
  !
  ! subroutine xy2pix_nest                          Done (to be tested)               
  ! subroutine pix2xy_nest                          Done (to be tested)
  ! subroutine mk_pix2xy                                  OK
  ! subroutine mk_xy2pix                                  OK
  ! subroutine mk_xy2pix1                                 OK
  ! subroutine neighbours_nest                      Done (to be tested)               
  ! subroutine next_in_line_nest                          Done  depends on xy2pix_nest pix2xy_nest             
  ! subroutine ang2vec                                    OK
  ! subroutine vec2ang                                    OK
  ! function nside2npix                            Done  
  ! subroutine surface_triangle                           OK
  ! subroutine angdist                                    OK
  ! subroutine vect_prod                                  OK
  ! function nside2ntemplates                       Done
  ! subroutine template_pixel_ring                  Done (to be tested)               
  ! subroutine same_shape_pixels_ring               Done (to be tested)               
  ! subroutine template_pixel_nest                  Done (to be tested)               
  ! subroutine same_shape_pixels_nest               Done (to be tested)               
!
! subroutine pix2ang_ring_old
! subroutine pix2vec_ring_old
! subroutine ang2pix_ring_old
! subroutine vec2pix_ring_old
! subroutine pix2ang_nest_old
! subroutine pix2vec_nest_old
! subroutine ang2pix_nest_old
! subroutine vec2pix_nest_old
! subroutine nest2ring_old
! subroutine ring2nest_old
! subroutine template_pixel_ring_old    
! subroutine same_shape_pixels_ring_old 
! subroutine template_pixel_nest_old    
! subroutine same_shape_pixels_nest_old 

  USE healpix_types
  USE misc_utils
  USE long_intrinsic, only: long_size
  IMPLICIT none

  INTEGER(KIND=i4b), private, PARAMETER :: ns_max4=8192     ! 2^13
  INTEGER(KIND=i4b), private, PARAMETER :: ns_max8=268435456! 2^28
#ifdef NO64BITS
  INTEGER(KIND=i4b), private, PARAMETER :: ns_max=ns_max4 ! largest nside available
#else
  INTEGER(KIND=i4b), private, PARAMETER :: ns_max=ns_max8 ! largest nside available
#endif

  !initialise array x2pix, y2pix and pix2x, pix2y used in several routines
  integer(KIND=i4b), private, save, dimension(128) :: x2pix=0,y2pix=0
  integer(KIND=i4b), private, save, dimension(0:127) :: x2pix1=0,y2pix1=0

  integer(KIND=i4b), private, save, dimension(0:1023) :: pix2x=0, pix2y=0

  ! coordinate of the lowest corner of each face
  INTEGER(KIND=I4B), private, save, dimension(0:11) :: jrll1 = (/ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 /) ! in unit of nside
  INTEGER(KIND=I4B), private, save, dimension(0:11) :: jpll1 = (/ 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 /) ! in unit of nside/2

  ! obsolete
  interface convert_inplace_real
     module procedure convert_inplace_real_1d, convert_inplace_real_nd
  end interface 
  interface convert_inplace_int
     module procedure convert_inplace_int_1d, convert_inplace_int_nd
  end interface 

  ! generic form
  interface convert_inplace
     module procedure convert_inplace_real_1d, convert_inplace_real_nd, &
          &           convert_inplace_int_1d, convert_inplace_int_nd, &
          &           convert_inplace_double_1d, convert_inplace_double_nd
  end interface 

  interface remove_dipole
     module procedure remove_dipole_real, remove_dipole_double, &
          &           remove_dipole_real_old, remove_dipole_double_old, &
          &           remove_dipole_real_v12, remove_dipole_double_v12
  end interface

  interface add_dipole
     module procedure add_dipole_real, add_dipole_double
  end interface

  interface convert_nest2ring
     module procedure convert_nest2ring_int_1d, &
          &           convert_nest2ring_real_1d, &
          &           convert_nest2ring_double_1d, &
          &           convert_nest2ring_int_nd, &
          &           convert_nest2ring_real_nd, &
          &           convert_nest2ring_double_nd
#ifndef NO64BITS
     module procedure convert_nest2ring_int8_1d, &
          convert_nest2ring_int8_nd
#endif
  end interface 

  interface convert_ring2nest
     module procedure convert_ring2nest_int_1d, &
          &           convert_ring2nest_real_1d, &
          &           convert_ring2nest_double_1d, &
          &           convert_ring2nest_int_nd, &
          &           convert_ring2nest_real_nd, &
          &           convert_ring2nest_double_nd
#ifndef NO64BITS
     module procedure convert_ring2nest_int8_1d, &
          convert_ring2nest_int8_nd
#endif
  end interface 

  interface medfiltmap
     module procedure medfiltmap_s, medfiltmap_d
  end interface 

#ifdef NO64BITS
  interface npix2nside
     module procedure npix2nside
  end interface
  interface ang2pix_nest
     module procedure ang2pix_nest
  end interface
  interface vec2pix_nest
     module procedure vec2pix_nest
  end interface
  interface ang2pix_ring
     module procedure ang2pix_ring
  end interface
  interface vec2pix_ring
     module procedure vec2pix_ring
  end interface
  interface ring2nest
     module procedure ring2nest
  end interface
  interface nest2ring
     module procedure nest2ring
  end interface
  interface pix2ang_ring
     module procedure pix2ang_ring
  end interface
  interface pix2vec_ring
     module procedure pix2vec_ring
  end interface
  interface pix2ang_nest
     module procedure pix2ang_nest
  end interface
  interface pix2vec_nest
     module procedure pix2vec_nest
  end interface
  interface in_ring
     module procedure in_ring
  end interface
  interface getdisc_ring
     module procedure getdisc_ring
  end interface
  interface query_strip
     module procedure query_strip
  end interface
  interface query_disc
     module procedure query_disc
  end interface
  interface query_polygon
     module procedure query_polygon
  end interface
  interface query_triangle
     module procedure query_triangle
  end interface
  interface xy2pix_nest
     module procedure xy2pix_nest
  end interface
  interface pix2xy_nest
     module procedure pix2xy_nest
  end interface
  interface neighbours_nest
     module procedure neighbours_nest
  end interface
  interface next_in_line_nest
     module procedure next_in_line_nest
  end interface
  interface template_pixel_ring
     module procedure template_pixel_ring
  end interface
  interface same_shape_pixels_ring
     module procedure same_shape_pixels_ring
  end interface
  interface template_pixel_nest
     module procedure template_pixel_nest
  end interface
  interface same_shape_pixels_nest
     module procedure same_shape_pixels_nest
  end interface
#else
  interface npix2nside
     module procedure npix2nside, npix2nside_8
  end interface
  interface ang2pix_nest
     module procedure ang2pix_nest, ang2pix_nest_8
  end interface
  interface vec2pix_nest
     module procedure vec2pix_nest, vec2pix_nest_8
  end interface
  interface ang2pix_ring
     module procedure ang2pix_ring, ang2pix_ring_8
  end interface
  interface vec2pix_ring
     module procedure vec2pix_ring, vec2pix_ring_8
  end interface
  interface ring2nest
     module procedure ring2nest, ring2nest_8
  end interface
  interface nest2ring
     module procedure nest2ring, nest2ring_8
  end interface
  interface pix2ang_ring
     module procedure pix2ang_ring, pix2ang_ring_8
  end interface
  interface pix2vec_ring
     module procedure pix2vec_ring, pix2vec_ring_8
  end interface
  interface pix2ang_nest
     module procedure pix2ang_nest, pix2ang_nest_8
  end interface
  interface pix2vec_nest
     module procedure pix2vec_nest, pix2vec_nest_8
  end interface
  interface in_ring
     module procedure in_ring, in_ring_8
  end interface
  interface getdisc_ring
     module procedure getdisc_ring, getdisc_ring_8
  end interface
  interface query_strip
     module procedure query_strip, query_strip_8
  end interface
  interface query_disc
     module procedure query_disc, query_disc_8
  end interface
  interface query_polygon
     module procedure query_polygon, query_polygon_8
  end interface
  interface query_triangle
     module procedure query_triangle, query_triangle_8
  end interface
  interface xy2pix_nest
     module procedure xy2pix_nest, xy2pix_nest_8
  end interface
  interface pix2xy_nest
     module procedure pix2xy_nest, pix2xy_nest_8
  end interface
  interface neighbours_nest
     module procedure neighbours_nest, neighbours_nest_8
  end interface
  interface next_in_line_nest
     module procedure next_in_line_nest, next_in_line_nest_8
  end interface
  interface template_pixel_ring
     module procedure template_pixel_ring, template_pixel_ring_8
  end interface
  interface same_shape_pixels_ring
     module procedure same_shape_pixels_ring, same_shape_pixels_ring_8
  end interface
  interface template_pixel_nest
     module procedure template_pixel_nest, template_pixel_nest_8
  end interface
  interface same_shape_pixels_nest
     module procedure same_shape_pixels_nest, same_shape_pixels_nest_8
  end interface
#endif

  private

  public :: remove_dipole, add_dipole, &
       & query_strip, &
       & query_polygon, &
       & query_triangle, &
       & query_disc, &
       & pix2ang_ring, pix2vec_ring, ang2pix_ring, vec2pix_ring, &
       & pix2ang_nest, pix2vec_nest, ang2pix_nest, vec2pix_nest, &
       & convert_nest2ring, convert_ring2nest, &
       & convert_inplace, convert_inplace_real, convert_inplace_int, &
       & nest2ring, ring2nest, xy2pix_nest, pix2xy_nest, &
       & mk_pix2xy, mk_xy2pix, &
       & neighbours_nest, &
       & next_in_line_nest, &
       & ang2vec, vec2ang, &
       & npix2nside, nside2npix, &
       & surface_triangle, angdist, vect_prod

  public :: ang2pix_nest_old, vec2pix_nest_old, &
       ang2pix_ring_old, vec2pix_ring_old, &
       ring2nest_old, nest2ring_old, &
       pix2ang_ring_old, pix2vec_ring_old, &
       pix2ang_nest_old, pix2vec_nest_old, &
       template_pixel_ring_old, same_shape_pixels_ring_old, &
       template_pixel_nest_old, same_shape_pixels_nest_old

  public :: nside2ntemplates, &
       & template_pixel_ring, same_shape_pixels_ring, &
       & template_pixel_nest, same_shape_pixels_nest

  public :: medfiltmap

  public :: intrs_intrv, in_ring, ring_num, ring2z ! arcane usage

  public :: getdisc_ring  ! obsolete


contains



  !=======================================================================
  function ring_num (nside, z, shift) result(ring_num_result)
    !=======================================================================
    ! ring = ring_num(nside, z [, shift=])
    !     returns the ring number in {1, 4*nside-1}
    !     from the z coordinate
    ! usually returns the ring closest to the z provided
    ! if shift < 0, returns the ring immediatly north (of smaller index) of z
    ! if shift > 0, returns the ring immediatly south (of smaller index) of z
    !
    !=======================================================================
    INTEGER(KIND=I4B)             :: ring_num_result
    REAL(KIND=DP),     INTENT(IN) :: z
    INTEGER(KIND=I4B), INTENT(IN) :: nside
    integer(i4b),      intent(in), optional :: shift

    INTEGER(KIND=I4B) :: iring
    real(DP) :: my_shift
    !=======================================================================


    my_shift = 0.0_dp
    if (present(shift)) my_shift = shift * 0.5_dp

    !     ----- equatorial regime ---------
    iring = NINT( nside*(2.0_dp-1.500_dp*z)   + my_shift )

    !     ----- north cap ------
    if (z > twothird) then
       iring = NINT( nside* SQRT(3.0_dp*(1.0_dp-z))  + my_shift )
       if (iring == 0) iring = 1
    endif

    !     ----- south cap -----
    if (z < -twothird   ) then
       ! beware that we do a -shift in the south cap
       iring = NINT( nside* SQRT(3.0_dp*(1.0_dp+z))   - my_shift )
       if (iring == 0) iring = 1
       iring = 4*nside - iring
    endif

    ring_num_result = iring

    return
  end function ring_num
  !=======================================================================
  function ring2z (nside, ir) result(z)
    !=======================================================================
    !     returns the z coordinate of ring ir for Nside
    ! 2009-03-25: accepts Nside > 8192
    !=======================================================================
    integer(kind=I4B), intent(in) :: ir
    integer(kind=I4B), intent(in) :: nside
    real(kind=DP)                 :: z

    real(DP)     :: fn, tmp
    !=======================================================================

    fn = real(nside,kind=DP)

    if (ir < nside) then  ! polar cap (north)
       tmp = real(ir, DP)
       z = 1.0_dp - (tmp * tmp) / (3.0_dp * fn * fn)
    else if (ir < 3*nside) then ! tropical band
       z = real( 2*nside-ir, kind=DP) * 2.0_dp / (3.0_dp * fn)
    else                  ! polar cap (south)
       tmp = real(4*nside - ir, DP)
       z = -1.0_dp + (tmp * tmp) / (3.0_dp * fn * fn)
    endif
    
    return
  end function ring2z
  !=======================================================================
  subroutine intrs_intrv( d1, d2, di, ni)
    !=======================================================================
    ! computes the intersection di
    ! of 2 intervals d1 (= [a1,b1]) and d2 (= [a2,b2])
    ! on the periodic domain ( = [A,B], where A and B are arbitrary)
    ! ni is the resulting number of intervals (0,1, or 2)
    !
    ! if a1<b1 then d1 = {x | a1 <= x <= b1}
    ! if a1>b1 then d1 = {x | a1 <= x <= B  U  A <= x <= b1}
    !=======================================================================
    real(kind=DP), dimension(1:), INTENT(IN)  :: d1, d2
    real(kind=DP), dimension(1:), INTENT(OUT) :: di
    integer(kind=I4B), INTENT(OUT) :: ni

    real(kind=DP), dimension(1:4) :: dk
    integer(kind=I4B) :: ik
    logical(kind=LGT) :: tr12, tr21, tr34, tr43, tr13, tr31, tr24, tr42, tr14, tr32
    !=======================================================================

    tr12 = (d1(1) < d1(2))
    tr21 = .NOT. tr12
    tr34 = (d2(1) < d2(2))
    tr43 = .NOT. tr34
    tr13 = (d1(1) < d2(1))
    tr31 = .NOT. tr13
    tr24 = (d1(2) < d2(2))
    tr42 = .NOT. tr24
    tr14 = (d1(1) < d2(2))
    tr32 = (d2(1) < d1(2))

    ik = 0
    dk(1:4) = -1.0e9_dp


    if ((tr34.AND.tr31.AND.tr14) .OR. (tr43.AND.(tr31.OR.tr14))) then
       ik = ik + 1
       dk(ik) = d1(1)  ! a1
    endif
    if ((tr12.AND.tr13.AND.tr32) .OR. (tr21.AND.(tr13.OR.tr32))) then
       ik = ik + 1
       dk(ik) = d2(1)  ! a2
    endif
    if ((tr34.AND.tr32.AND.tr24) .OR. (tr43.AND.(tr32.OR.tr24))) then
       ik = ik + 1
       dk(ik) = d1(2)  ! b1
    endif
    if ((tr12.AND.tr14.AND.tr42) .OR. (tr21.AND.(tr14.OR.tr42))) then
       ik = ik + 1
       dk(ik) =  d2(2)  ! b2
    endif

    di(1:4) = 0.0_dp
    select case (ik)
    case (0)
       ni = 0
    case (2)
       ni = 1
       di(1:2) = (/ dk(1), dk(2) /) ! [a1,b1] or [a1,b2] or [a2,b1] or [a2,b2]
    case (4)
       ni = 2
       di(1:4) = (/ dk(1), dk(4), dk(2), dk(3) /) ! [a1,b2] U [a2,b1]
    case default
       print*,"error in intrs_intrv", ik
       print*,dk
       print*,d1,d2
    end select

    return
  end subroutine intrs_intrv

  !=======================================================================
  subroutine correct_ring_phi(location, iring, iphi)
    !=======================================================================
    integer(i4b), intent(in)    :: location !+1:North, -1:South
    integer(i4b), intent(inout) :: iring, iphi
    integer(i4b) :: delta
    !-----------------------------------------------------------------------
    delta = 0
    if (iphi < 0)        delta =  +1
    if (iphi >= 4*iring) delta =  -1
    if (delta /= 0) then
       if (abs(location) /= 1) then 
          stop 'wrong location'
       endif
       iring = iring - location * delta
       iphi  = iphi  +            delta * (4*iring)
    endif
    return
  end subroutine correct_ring_phi

  ! perform 2 compilations of the same source file
#undef DOI8Bi
#include "pixel_routines.F90"
#ifndef NO64BITS
#define DOI8B
#include "pixel_routines.F90"
#endif
!
! pix2ang_ring
! pix2vec_ring
! ang2pix_ring
! vec2pix_ring
! pix2ang_nest
! pix2vec_nest
! ang2pix_nest
! vec2pix_nest
! nest2ring
! npix2nside

  !*************************************************************
  !
  !                    MAP Manipulations
  !
  !*************************************************************
  subroutine warning_oldbounds(code, cos_theta_cut, zbounds)
    character(len=*), intent(in)          :: code
    real(DP), intent(in)                  :: cos_theta_cut
    real(DP), intent(out), dimension(1:2) :: zbounds

    if (cos_theta_cut <= 0.0_dp) then ! no cut
       zbounds(1) =  -1.0_dp
       zbounds(2) =   1.0_dp
    else
       zbounds(1) =   cos_theta_cut
       zbounds(2) = - cos_theta_cut
    endif
    print*,' -------------------------------------'
    print*,'WARNING: obsolete interface to '//code
    print*,'    cos_theta_cut currently a DP scalar with value'
    write(*,9000) '    ',cos_theta_cut
    print*,'    shoud now be replaced with a 2-element vector with values:'
    write(*,9001) '    ',zbounds(1),zbounds(2)
    print*,'    See documentation for details.'
    print*,' -------------------------------------'
9000 format (a,g12.6)
9001 format (a,g12.6,g12.6)

    return
  end subroutine warning_oldbounds
  !==============================================================
  ! REMOVE_DIPOLE( nside, map, ordering, degree, multipoles, zbounds, fmissval, mask, weights)
  !
  ! removes monopole (and dipole) from a map
  !
  ! Nside:     I4,       IN   : Healpix resolution parameter
  ! map:       KMAP, array,INOUT: Heapix map (see Notes below)
  ! ordering:  I4,       IN:   Healpix scheme 1:RING, 2: NESTED
  ! degree:    I4,       IN:   multipole to remove, 1: monopole, 2: monopole and dipole
  ! multipoles:R8, array,OUT:  value of monopole and dipole
  ! zbounds   :R8,      IN:  2-el vector
  ! range of z in [-1,1] on which to estimate monopole and dipole
  ! fmissval:  KMAP, Option, IN: value used to flag bad pixel on input, default=-1.6375e30
  !                            Pixels with map = fmissval are not used for fit
  ! mask    :  KMAP, Option, IN: Pixels with |mask|<1.e-10 are not used for fit
  !                              others are kept as is
  !                               note : the mask in NOT applied to the map
  ! weights  : KMAP, Option, IN: pixel weight to be applied to the map
  !
  ! KMAP: either R4 or R8
  !
  ! note : if degree= 1, or 2, the map is modified on output
  !     * the monopole (and dipole) is/are removed
  !     * pixels within the symmetric cut parameterized
  !       by cos_theta_cut are set to fmissval (or its default value)
  !  if degree = 0, nothing is done
  !  all other values of degree are invalid
  !
  ! v1.0, EH, Caltech, Jan-2002, based on homonyme IDL routine
  ! 2008-02-03: addition of weights
  !
  !==============================================================
  subroutine remove_dipole_real( nside, map, ordering, degree, multipoles, zbounds, &
       & fmissval, mask, weights)
    !============================================================
    use num_rec, only : dsvdcmp, dsvbksb
    ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_REAL"
    integer(kind=I4B),     parameter :: KMAP = SP
    real   (kind=KMAP),    parameter :: fbad_value = -1.6375e30_KMAP
    !
    include 'remove_dipole_inc.f90'
  end subroutine remove_dipole_real
  !==============================================================
  subroutine remove_dipole_double( nside, map, ordering, degree, multipoles, zbounds, &
       & fmissval, mask, weights)
    !============================================================
    use num_rec, only : dsvdcmp, dsvbksb
    ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_DOUBLE"
    integer(kind=I4B),     parameter :: KMAP = DP
    real   (kind=KMAP),    parameter :: fbad_value = -1.6375e30_KMAP
    !
    include 'remove_dipole_inc.f90'
  end subroutine remove_dipole_double

  !==============================================================
  subroutine remove_dipole_real_old( nside, map, ordering, degree, multipoles, cos_theta_cut, fmissval, mask)
    !============================================================
    ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_REAL"
    integer(kind=I4B),     parameter :: KMAP = SP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree
    real   (kind=DP),   dimension(0:),  intent(out)   :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=DP),                   intent(in)    :: cos_theta_cut
    real   (kind=KMAP),                 intent(in), optional :: fmissval
    real   (kind=KMAP), dimension(0:),  intent(in), optional :: mask
    ! local
    real(DP),                          dimension(1:2) :: zbounds

    call warning_oldbounds(code, cos_theta_cut, zbounds)
    call remove_dipole(nside, map, ordering, degree, multipoles, zbounds, fmissval, mask)
  end subroutine remove_dipole_real_old
  !==============================================================
  subroutine remove_dipole_double_old( nside, map, ordering, degree, multipoles, cos_theta_cut, fmissval, mask)
    !============================================================
     ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_DOUBLE"
    integer(kind=I4B),     parameter :: KMAP = DP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree
    real   (kind=DP),   dimension(0:),  intent(out)   :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=DP),                   intent(in)    :: cos_theta_cut
    real   (kind=KMAP),                 intent(in), optional :: fmissval
    real   (kind=KMAP), dimension(0:),  intent(in), optional :: mask
    ! local
    real(DP),                          dimension(1:2) :: zbounds

    call warning_oldbounds(code, cos_theta_cut, zbounds)
    call remove_dipole(nside, map, ordering, degree, multipoles, zbounds, fmissval, mask) 
  end subroutine remove_dipole_double_old

  !==============================================================
  subroutine remove_dipole_real_v12( nside, map, nmaps, ordering, degree, multipoles, cos_theta_cut, fmissval, mask)
    !============================================================
    ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_REAL"
    integer(kind=I4B),     parameter :: KMAP = SP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree, nmaps
    real   (kind=DP),   dimension(0:),  intent(out)   :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=DP),                   intent(in)    :: cos_theta_cut
    real   (kind=KMAP),                 intent(in), optional :: fmissval
    real   (kind=KMAP), dimension(0:),  intent(in), optional :: mask
    ! local
    real(DP),                          dimension(1:2) :: zbounds

    print*,'=========================================================='
    print*,'WARNING: Interface to remove_dipole has changed'
    print*,' from'
    print*,'call remove_dipole(nside, map, NMAPS, ordering, degree, multipoles, COS_THETA_CUT, fmissval, mask)'
    print*,' to'
    print*,'call remove_dipole(nside, map,        ordering, degree, multipoles, ZBOUNDS,       fmissval, mask)'
    print*,'=========================================================='
    
    call warning_oldbounds(code, cos_theta_cut, zbounds)
    call remove_dipole(nside, map, ordering, degree, multipoles, zbounds, fmissval, mask)
  end subroutine remove_dipole_real_v12
  !==============================================================
  subroutine remove_dipole_double_v12( nside, map, nmaps, ordering, degree, multipoles, cos_theta_cut, fmissval, mask)
    !============================================================
     ! parameters
    character(len=*),      parameter :: code = "REMOVE_DIPOLE_DOUBLE"
    integer(kind=I4B),     parameter :: KMAP = DP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree, nmaps
    real   (kind=DP),   dimension(0:),  intent(out)   :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=DP),                   intent(in)    :: cos_theta_cut
    real   (kind=KMAP),                 intent(in), optional :: fmissval
    real   (kind=KMAP), dimension(0:),  intent(in), optional :: mask
    ! local
    real(DP),                          dimension(1:2) :: zbounds

    print*,'=========================================================='
    print*,'WARNING: Interface to remove_dipole has changed'
    print*,' from'
    print*,'call remove_dipole(nside, map, NMAPS, ordering, degree, multipoles, COS_THETA_CUT, fmissval, mask)'
    print*,' to'
    print*,'call remove_dipole(nside, map,        ordering, degree, multipoles, ZBOUNDS,       fmissval, mask)'
    print*,'=========================================================='

    call warning_oldbounds(code, cos_theta_cut, zbounds)
    call remove_dipole(nside, map, ordering, degree, multipoles, zbounds, fmissval, mask) 
  end subroutine remove_dipole_double_v12

  !=======================================================================
  ! ADD_DIPOLE( nside, map, ordering, degree, multipoles, fmissval)
  !
  ! removes monopole (and dipole) from a map
  !
  ! Nside:     I4,       IN   : Healpix resolution parameter
  ! map:       KMAP, array,INOUT: Heapix map (see Notes below)
  ! ordering:  I4,       IN:   Healpix scheme 1:RING, 2: NESTED
  ! degree:    I4,       IN:   multipole to remove, 1: monopole, 2: monopole and dipole
  ! multipoles:R8, array,IN:  value of monopole and dipole
  ! fmissval:  KMAP, Option, IN: value used to flag bad pixel on input, default=-1.6375e30
  !                            Pixels with map = fmissval are left unchanged
  ! 2009-03-25: accepts Nside > 8192
  !=======================================================================
  subroutine add_dipole_real(nside, map, ordering, degree, multipoles, fmissval)
    !=======================================================================
    ! single precision
    !=======================================================================
    character(len=*),      parameter :: code = "ADD_DIPOLE_REAL"
    integer(kind=I4B),     parameter :: KMAP = SP
    real   (kind=KMAP),    parameter :: fbad_value = -1.6375e30_KMAP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree
    real   (kind=DP),   dimension(0:),  intent(in)    :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=KMAP),                 intent(in), optional :: fmissval

    real   (kind=KMAP)                :: fmiss_effct
    integer(kind=i8b)                 :: ipix, npix
    logical(lgt)                      :: dodipole !, do_mask
    real(kind=dp), dimension(1:3)     :: vec
    !=======================================================================
    
    npix = nside2npix(nside)
    fmiss_effct = fbad_value
    if (present(fmissval)) fmiss_effct = fmissval

    if (degree == 0) then
       print*," No monopole nor dipole to add"
       return
    elseif (degree == 1) then
       dodipole = .false.
    else if (degree == 2) then
       dodipole = .true.
    else
       print*,code//"> degree can only be "
       print*,"      1: monopole (l=0) addition or "
       print*,"      2: monopole and dipole (l=0,1) addition"
       print*,code//"> ABORT ! "
       call fatal_error
    endif

    do ipix = 0, npix-1
       if ( abs(map(ipix) - fmiss_effct) <= abs(1.e-5*fmiss_effct) ) goto 20
!        if (do_mask) then
!           if (abs(mask(ipix)) <= 1.e-10) goto 20
!        endif
       map(ipix) = map(ipix) + multipoles(0)
       if (dodipole) then
          ! computes dipole basis functions
          ! pixel -> vector
          if (ordering == 1) call pix2vec_ring( nside, ipix, vec)
          if (ordering == 2) call pix2vec_nest( nside, ipix, vec)
          map(ipix) = map(ipix) + sum(multipoles(1:3) * vec(1:3))
       endif

20     continue
    enddo

    return
  end subroutine add_dipole_real
  !=======================================================================
  subroutine add_dipole_double(nside, map, ordering, degree, multipoles, fmissval)
    !=======================================================================
    ! single precision
    !=======================================================================
    character(len=*),      parameter :: code = "ADD_DIPOLE_DOUBLE"
    integer(kind=I4B),     parameter :: KMAP = DP
    real   (kind=KMAP),    parameter :: fbad_value = -1.6375e30_KMAP
    ! dummy
    integer(kind=i4b),                  intent(in)    :: nside
    integer(kind=i4b),                  intent(in)    :: ordering, degree
    real   (kind=DP),   dimension(0:),  intent(in)    :: multipoles
    real   (kind=KMAP), dimension(0:),  intent(inout) :: map
    real   (kind=KMAP),                 intent(in), optional :: fmissval

    real   (kind=KMAP)                :: fmiss_effct
    integer(kind=i8b)                 :: ipix, npix
    logical(lgt)                      :: dodipole !, do_mask
    real(kind=dp), dimension(1:3)     :: vec
    !=======================================================================
    
    npix = nside2npix(nside)
    fmiss_effct = fbad_value
    if (present(fmissval)) fmiss_effct = fmissval

    if (degree == 0) then
       print*," No monopole nor dipole to add"
       return
    elseif (degree == 1) then
       dodipole = .false.
    else if (degree == 2) then
       dodipole = .true.
    else
       print*,code//"> degree can only be "
       print*,"      1: monopole (l=0) addition or "
       print*,"      2: monopole and dipole (l=0,1) addition"
       print*,code//"> ABORT ! "
       call fatal_error
    endif

    do ipix = 0, npix-1
       if ( abs(map(ipix) - fmiss_effct) <= abs(1.e-5*fmiss_effct) ) goto 20
!        if (do_mask) then
!           if (abs(mask(ipix)) <= 1.e-10) goto 20
!        endif
       map(ipix) = map(ipix) + multipoles(0)
       if (dodipole) then
          ! computes dipole basis functions
          ! pixel -> vector
          if (ordering == 1) call pix2vec_ring( nside, ipix, vec)
          if (ordering == 2) call pix2vec_nest( nside, ipix, vec)
          map(ipix) = map(ipix) + sum(multipoles(1:3) * vec(1:3))
       endif

20     continue
    enddo

    return
  end subroutine add_dipole_double
  !====================================================
  ! medfiltmap
  !   compute the median filtered map of a given Healpix map
  !   in_map: SP/DP  input Healpix full sky map
  !   radius: DP     radius in Radians
  !   med_map: SP/DP output Healpix full sky map
  !   nest:    I4B, OPT   either 0 (ring scheme) or 1 (nested scheme)
  !   fmissval:SP/DP, OPT sentinel value given to missing pixels
  !   fill_holes: LGT, OPT 
  !=================================================================
  subroutine medfiltmap_S( in_map, radius, med_map, nest, fmissval, fill_holes)
  !=================================================================
    use statistics, only: median
    integer(I4B), parameter :: KMAP = SP
    !
    real(KMAP), dimension(0:),intent(in), target       :: in_map
    real(DP),                 intent(in)               :: radius
    real(KMAP), dimension(0:),intent(out)              :: med_map
    integer(I4B),             intent(in),  optional    :: nest
    real(KMAP),               intent(in),  optional    :: fmissval
    logical(LGT),             intent(in),  optional    :: fill_holes
    !
    integer(I8B) :: npix, np
    integer(I4B) :: nside, p, nlist, status
    logical(LGT) :: do_nest, do_fill
    real(DP)     :: fraction
    real(KMAP)   :: fmissval_in
    integer(I4B), dimension(:),  allocatable :: listpix
    real(DP),     dimension(1:3)             :: vector
    character(len=*), parameter :: code = 'medfiltmap'
    !-----------------------------------------------
    npix = long_size(in_map)
    nside = npix2nside(npix)
    call assert(nside > 0, code//": invalid map size")

    fraction = 0.5_DP * (1.0_dp - cos(radius))
    np = npix * fraction * 1.2 + 50
    call assert(np < MAX_I4B, code//": too many pixels to compute median")

    allocate(listpix(0:np-1),stat=status)
    call assert_alloc(status,code,'listpix')

    do_nest = .false.
    if (present(nest)) then
       call assert(nest>=0 .and. nest <=1,code//': invalid NEST flag')
       do_nest = (nest == 1)
    endif

    do_fill = .false.
    if (present(fill_holes)) do_fill = fill_holes

    fmissval_in = hpx_Sbadval
    if (present(fmissval)) fmissval_in = fmissval

    do p = 0, npix-1
       ! find pixel location
       if (do_nest) then
          call pix2vec_nest( nside, p, vector)
       else
          call pix2vec_ring( nside, p, vector)
       endif

       ! find disc centered on pixel
       call query_disc(nside, vector, radius, listpix, nlist, nest=nest)

       if (do_fill .or. abs(in_map(p)-fmissval_in) > abs(fmissval_in*1.e-7)) then
          med_map(p) = median(in_map(listpix(0:nlist-1)), badval = fmissval_in, even= .true.)
       else
          med_map(p) = in_map(p)
       endif
    enddo

    deallocate(listpix)
    return
  end subroutine medfiltmap_S
  !=================================================================
  subroutine medfiltmap_D( in_map, radius, med_map, nest, fmissval, fill_holes)
  !=================================================================
    use statistics, only: median
    integer(I4B), parameter :: KMAP = DP
    !
    real(KMAP), dimension(0:),intent(in), target       :: in_map
    real(DP),                 intent(in)               :: radius
    real(KMAP), dimension(0:),intent(out)              :: med_map
    integer(I4B),             intent(in),  optional    :: nest
    real(KMAP),               intent(in),  optional    :: fmissval
    logical(LGT),             intent(in),  optional    :: fill_holes
    !
    integer(I8B) :: npix, np
    integer(I4B) :: nside, p, nlist, status
    logical(LGT) :: do_nest, do_fill
    real(DP)     :: fraction
    real(KMAP)   :: fmissval_in
    integer(I4B), dimension(:),  allocatable :: listpix
    real(DP),     dimension(1:3)             :: vector
    character(len=*), parameter :: code = 'medfiltmap'
    !-----------------------------------------------
    npix = long_size(in_map)
    nside = npix2nside(npix)
    call assert(nside > 0, code//": invalid map size")

    fraction = 0.5_DP * (1.0_dp - cos(radius))
    np = npix * fraction * 1.2 + 50
    call assert(np < MAX_I4B, code//": too many pixels to compute median")

    allocate(listpix(0:np-1),stat=status)
    call assert_alloc(status,code,'listpix')

    do_nest = .false.
    if (present(nest)) then
       call assert(nest>=0 .and. nest <=1,code//': invalid NEST flag')
       do_nest = (nest == 1)
    endif

    do_fill = .false.
    if (present(fill_holes)) do_fill = fill_holes

    fmissval_in = hpx_Dbadval
    if (present(fmissval)) fmissval_in = fmissval

    do p = 0, npix-1
       ! find pixel location
       if (do_nest) then
          call pix2vec_nest( nside, p, vector)
       else
          call pix2vec_ring( nside, p, vector)
       endif

       ! find disc centered on pixel
       call query_disc(nside, vector, radius, listpix, nlist, nest=nest)

       if (do_fill .or. abs(in_map(p)-fmissval_in) > abs(fmissval_in*1.e-7)) then
          med_map(p) = median(in_map(listpix(0:nlist-1)), badval = fmissval_in, even= .true.)
       else
          med_map(p) = in_map(p)
       endif
    enddo

    deallocate(listpix)
    return
  end subroutine medfiltmap_D
  !**************************************************************
  ! following 2x3 routines: out of place conversions for 1D maps
  ! if size(map) = Npix, peak memory = 2*Npix
  ! These routines are parallelized for shared memory architecture 
  !   (using OpenMP directives)
  !**************************************************************
  !=======================================================================
  !     makes the conversion NEST to RING
  !=======================================================================
  subroutine convert_nest2ring_int_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_nest2ring_int_1d'
    integer(kind=I4B),   parameter :: KMAP = I4B
    integer(kind=KMAP),  dimension(0:), intent(inout) ::  map
    integer(kind=KMAP),  dimension(:),  allocatable :: map_tmp
    include 'convert_nest2ring_1d_inc.f90'
  end subroutine convert_nest2ring_int_1d
#ifndef NO64BITS
  !=======================================================================
  subroutine convert_nest2ring_int8_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_nest2ring_int8_1d'
    integer(kind=I4B),   parameter :: KMAP = I8B
    integer(kind=KMAP),  dimension(0:), intent(inout) ::  map
    integer(kind=KMAP),  dimension(:),  allocatable :: map_tmp
    include 'convert_nest2ring_1d_inc.f90'
  end subroutine convert_nest2ring_int8_1d
#endif
  !=======================================================================
  subroutine convert_nest2ring_real_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_nest2ring_real_1d'
    integer(kind=I4B),   parameter :: KMAP = SP
    real   (kind=KMAP),  dimension(0:), intent(inout) ::  map
    real   (kind=KMAP),  dimension(:),  allocatable :: map_tmp
    include 'convert_nest2ring_1d_inc.f90'
  end subroutine convert_nest2ring_real_1d
  !=======================================================================
  subroutine convert_nest2ring_double_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_nest2ring_double_1d'
    integer(kind=I4B),   parameter :: KMAP = DP
    real   (kind=KMAP),  dimension(0:), intent(inout) ::  map
    real   (kind=KMAP),  dimension(:),  allocatable :: map_tmp
    include 'convert_nest2ring_1d_inc.f90'
  end subroutine convert_nest2ring_double_1d
  !=======================================================================
  !     makes the conversion RING to NEST
  !=======================================================================
  subroutine convert_ring2nest_int_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_ring2nest_int_1d'
    integer(kind=I4B),   parameter :: KMAP = I4B
    integer(kind=KMAP),  dimension(0:), intent(inout) ::  map
    integer(kind=KMAP),  dimension(:), allocatable :: map_tmp
    include 'convert_ring2nest_1d_inc.f90'
  end subroutine convert_ring2nest_int_1d
#ifndef NO64BITS
  !=======================================================================
  subroutine convert_ring2nest_int8_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_ring2nest_int8_1d'
    integer(kind=I4B),   parameter :: KMAP = I8B
    integer(kind=KMAP),  dimension(0:), intent(inout) ::  map
    integer(kind=KMAP),  dimension(:), allocatable :: map_tmp
    include 'convert_ring2nest_1d_inc.f90'
  end subroutine convert_ring2nest_int8_1d
#endif
  !=======================================================================
  subroutine convert_ring2nest_real_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_ring2nest_real_1d'
    integer(kind=I4B),   parameter :: KMAP = SP
    real   (kind=KMAP),  dimension(0:), intent(inout) ::  map
    real   (kind=KMAP),  dimension(:), allocatable :: map_tmp
    include 'convert_ring2nest_1d_inc.f90'
  end subroutine convert_ring2nest_real_1d
  !=======================================================================
  subroutine convert_ring2nest_double_1d(nside, map)
    !=======================================================================
    character(len=*), parameter :: code = 'convert_ring2nest_double_1d'
    integer(kind=I4B),   parameter :: KMAP = DP
    real   (kind=KMAP),  dimension(0:), intent(inout) ::  map
    real   (kind=KMAP),  dimension(:), allocatable :: map_tmp
    include 'convert_ring2nest_1d_inc.f90'
  end subroutine convert_ring2nest_double_1d

  !**************************************************************
  ! following 6 routines: out of place conversions for N-Dim maps
  ! if size(map) = Npix*Nd, peak memory = (Nd+2)*Npix
  ! 2004-08-25, EH
  !**************************************************************
  !=======================================================================
  subroutine convert_nest2ring_int_nd(nside, map)
    !=======================================================================
    !   NEST to RING conversion: 1D, integer
    !=======================================================================
    character(len=*),  parameter :: code = "convert_nest2ring_int_nd"
    integer(kind=I4B), parameter :: KMAP = I4B
    integer(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    integer(kind=KMAP), dimension(:),               allocatable :: map_tmp
    integer(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_nest2ring_nd_inc.f90'
  end subroutine convert_nest2ring_int_nd
#ifndef NO64BITS
  !=======================================================================
  subroutine convert_nest2ring_int8_nd(nside, map)
    !=======================================================================
    !   NEST to RING conversion: 1D, integer*8
    !=======================================================================
    character(len=*),  parameter :: code = "convert_nest2ring_int_nd"
    integer(kind=I4B), parameter :: KMAP = I8B
    integer(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    integer(kind=KMAP), dimension(:),               allocatable :: map_tmp
    integer(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_nest2ring_nd_inc.f90'
  end subroutine convert_nest2ring_int8_nd
#endif
  !=======================================================================
  subroutine convert_nest2ring_real_nd(nside, map)
    !=======================================================================
    !   NEST to RING conversion: 1D, real
    !=======================================================================
    character(len=*),  parameter :: code = "convert_nest2ring_real_nd"
    integer(kind=I4B), parameter :: KMAP = SP
    real(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    real(kind=KMAP), dimension(:),               allocatable :: map_tmp
    real(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_nest2ring_nd_inc.f90'
  end subroutine convert_nest2ring_real_nd
  !=======================================================================
  subroutine convert_nest2ring_double_nd(nside, map)
    !=======================================================================
    !   NEST to RING conversion: 1D, double
    !=======================================================================
    character(len=*),  parameter :: code = "convert_nest2ring_double_nd"
    integer(kind=I4B), parameter :: KMAP = DP
    real(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    real(kind=KMAP), dimension(:),               allocatable :: map_tmp
    real(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_nest2ring_nd_inc.f90'
  end subroutine convert_nest2ring_double_nd
  !=======================================================================
  subroutine convert_ring2nest_int_nd(nside, map)
    !=======================================================================
    !   RING to NEST conversion: 1D, integer
    !=======================================================================
    character(len=*),  parameter :: code = "convert_ring2nest_int_nd"
    integer(kind=I4B), parameter :: KMAP = I4B
    integer(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    integer(kind=KMAP), dimension(:),               allocatable :: map_tmp
    integer(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_ring2nest_nd_inc.f90'
  end subroutine convert_ring2nest_int_nd
#ifndef NO64BITS
  !=======================================================================
  subroutine convert_ring2nest_int8_nd(nside, map)
    !=======================================================================
    !   RING to NEST conversion: 1D, integer
    !=======================================================================
    character(len=*),  parameter :: code = "convert_ring2nest_int8_nd"
    integer(kind=I4B), parameter :: KMAP = I8B
    integer(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    integer(kind=KMAP), dimension(:),               allocatable :: map_tmp
    integer(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_ring2nest_nd_inc.f90'
  end subroutine convert_ring2nest_int8_nd
#endif
  !=======================================================================
  subroutine convert_ring2nest_real_nd(nside, map)
    !=======================================================================
    !   RING to NEST conversion: 1D, real
    !=======================================================================
    character(len=*),  parameter :: code = "convert_ring2nest_real_nd"
    integer(kind=I4B), parameter :: KMAP = SP
    real(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    real(kind=KMAP), dimension(:),               allocatable :: map_tmp
    real(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_ring2nest_nd_inc.f90'
  end subroutine convert_ring2nest_real_nd
  !=======================================================================
  subroutine convert_ring2nest_double_nd(nside, map)
    !=======================================================================
    !   RING to NEST conversion: 1D, double
    !=======================================================================
    character(len=*),  parameter :: code = "convert_ring2nest_double_nd"
    integer(kind=I4B), parameter :: KMAP = DP
    real(kind=KMAP), dimension(0:,1:), intent(inout), target ::  map
    real(kind=KMAP), dimension(:),               allocatable :: map_tmp
    real(kind=KMAP), dimension(:),                   pointer :: map1
    include 'convert_ring2nest_nd_inc.f90'
  end subroutine convert_ring2nest_double_nd
  !====================================================================
  ! The following 6 routines convert in place integer, real, and double
  ! arrays between the NESTED and RING schemes.
  !
  ! in place: without allocating a temporary map. This routine is more general,
  ! but slower than convert_nest2ring.
  !
  !     This is a wrapper for the toolbox functions "ring2nest" and
  !     "nest2ring". Their names are supplied in the "subcall"
  !     argument.
  !
  ! Author: Benjamin D. Wandelt October 1997
  ! Added to pix_tools for version 1.00 in March 1999.
  ! 2004-08-25: EH, added N-Dim facility and double precision IO
  !====================================================================
  !====================================================================
  subroutine convert_inplace_int_1d(subcall,map)
    !==================================================================
    ! 1D, integer implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_int_1d"
    integer(kind=I4B), parameter :: KMAP = I4B
    integer(kind=KMAP), dimension(0:)    :: map
    integer(kind=KMAP)                   :: pixbuf1,pixbuf2
    include 'convert_inplace_1d_inc.f90'
  end subroutine convert_inplace_int_1d
  !====================================================================
  subroutine convert_inplace_real_1d(subcall,map)
    !====================================================================
    ! 1D, real implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_real_1d"
    integer(kind=I4B), parameter :: KMAP = SP
    real   (kind=KMAP), dimension(0:)    :: map
    real   (kind=KMAP)                   :: pixbuf1,pixbuf2
    include 'convert_inplace_1d_inc.f90'
  end subroutine convert_inplace_real_1d
  !====================================================================
  subroutine convert_inplace_double_1d(subcall,map)
    !====================================================================
    ! 1D, double precision implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_double_1d"
    integer(kind=I4B), parameter :: KMAP = DP
    real   (kind=KMAP), dimension(0:)    :: map
    real   (kind=KMAP)                   :: pixbuf1,pixbuf2
    include 'convert_inplace_1d_inc.f90'
  end subroutine convert_inplace_double_1d
  !====================================================================
  subroutine convert_inplace_int_nd(subcall,map)
    !==================================================================
    ! ND, integer implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_int_nd"
    integer(kind=i4b), parameter :: ND_MAX = 10
    integer(kind=I4B), parameter :: KMAP = I4B
    integer(kind=KMAP), dimension(0:,1:)    :: map
    integer(kind=KMAP), dimension(1:ND_MAX) :: pixbuf1,pixbuf2
    include 'convert_inplace_nd_inc.f90'
  end subroutine convert_inplace_int_nd
  !====================================================================
  subroutine convert_inplace_real_nd(subcall,map)
    !====================================================================
    ! ND, real implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_real_nd"
    integer(kind=i4b), parameter :: ND_MAX = 10
    integer(kind=I4B), parameter :: KMAP = SP
    real   (kind=KMAP), dimension(0:,1:)    :: map
    real   (kind=KMAP), dimension(1:ND_MAX) :: pixbuf1,pixbuf2
    include 'convert_inplace_nd_inc.f90'
  end subroutine convert_inplace_real_nd
  !====================================================================
  subroutine convert_inplace_double_nd(subcall,map)
    !====================================================================
    ! ND, double precision implementation
    !==================================================================
    character(len=*),  parameter :: code = "convert_inplace_double_nd"
    integer(kind=i4b), parameter :: ND_MAX = 10
    integer(kind=I4B), parameter :: KMAP = DP
    real   (kind=KMAP), dimension(0:,1:)    :: map
    real   (kind=KMAP), dimension(1:ND_MAX) :: pixbuf1,pixbuf2
    include 'convert_inplace_nd_inc.f90'
  end subroutine convert_inplace_double_nd
  !=======================================================================
  subroutine mk_pix2xy()
    !=======================================================================
    !     constructs the array giving x and y in the face from pixel number
    !     for the nested (quad-cube like) ordering of pixels
    !
    !     the bits corresponding to x and y are interleaved in the pixel number
    !     one breaks up the pixel number by even and odd bits
    !=======================================================================
    INTEGER(KIND=I4B) ::  kpix, jpix, ix, iy, ip, id

    !cc cf block data      data      pix2x(1023) /0/
    !-----------------------------------------------------------------------
    !      print *, 'initiate pix2xy'
    do kpix=0,1023          ! pixel number
       jpix = kpix
       IX = 0
       IY = 0
       IP = 1               ! bit position (in x and y)
!        do while (jpix/=0) ! go through all the bits
       do
          if (jpix == 0) exit ! go through all the bits
          ID = MODULO(jpix,2)  ! bit value (in kpix), goes in ix
          jpix = jpix/2
          IX = ID*IP+IX

          ID = MODULO(jpix,2)  ! bit value (in kpix), goes in iy
          jpix = jpix/2
          IY = ID*IP+IY

          IP = 2*IP         ! next bit (in x and y)
       enddo
       pix2x(kpix) = IX     ! in 0,31
       pix2y(kpix) = IY     ! in 0,31
    enddo

    return
  end subroutine mk_pix2xy
  !=======================================================================
  subroutine mk_xy2pix()
    !=======================================================================
    !     sets the array giving the number of the pixel lying in (x,y)
    !     x and y are in {1,128}
    !     the pixel number is in {0,128**2-1}
    !
    !     if  i-1 = sum_p=0  b_p * 2^p
    !     then ix = sum_p=0  b_p * 4^p
    !          iy = 2*ix
    !     ix + iy in {0, 128**2 -1}
    !=======================================================================
    INTEGER(KIND=I4B):: k,ip,i,j,id
    !=======================================================================

    do i = 1,128           !for converting x,y into
       j  = i-1            !pixel numbers
       k  = 0
       ip = 1

       do
          if (j==0) then
             x2pix(i) = k
             y2pix(i) = 2*k
             exit
          else
             id = MODULO(J,2)
             j  = j/2
             k  = ip*id+k
             ip = ip*4
          endif
       enddo

    enddo

    return
  end subroutine mk_xy2pix
  !=======================================================================
  subroutine mk_xy2pix1()
    !=======================================================================
    !     sets the array giving the number of the pixel lying in (x,y)
    !     x and y are in {1,128}
    !     the pixel number is in {0,128**2-1}
    !
    !     if  i-1 = sum_p=0  b_p * 2^p
    !     then ix = sum_p=0  b_p * 4^p
    !          iy = 2*ix
    !     ix + iy in {0, 128**2 -1}
    !=======================================================================
    INTEGER(KIND=I4B):: k,ip,i,j,id
    !=======================================================================

    do i = 0,127           !for converting x,y into
       j  = i           !pixel numbers
       k  = 0
       ip = 1

       do
          if (j==0) then
             x2pix1(i) = k
             y2pix1(i) = 2*k
             exit
          else
             id = MODULO(J,2)
             j  = j/2
             k  = ip*id+k
             ip = ip*4
          endif
       enddo

    enddo

    return
  end subroutine mk_xy2pix1
  !=======================================================================

  !=======================================================================
  subroutine ang2vec(theta, phi, vector)
    !=======================================================================
    !     renders the vector (x,y,z) corresponding to angles
    !     theta (co-latitude measured from North pole, in [0,Pi] radians)
    !     and phi (longitude measured eastward, in radians)
    !     North pole is (x,y,z)=(0,0,1)
    !     added by EH, Feb 2000
    !=======================================================================
    REAL(KIND=DP), INTENT(IN) :: theta, phi
    REAL(KIND=DP), INTENT(OUT), dimension(1:) :: vector

    REAL(KIND=DP) :: sintheta
    !=======================================================================

    if (theta<0.0_dp .or. theta>pi)  then
       print*,"ANG2VEC: theta : ",theta," is out of range [0, Pi]"
       call fatal_error
    endif
    sintheta = SIN(theta)

    vector(1) = sintheta * COS(phi)
    vector(2) = sintheta * SIN(phi)
    vector(3) = COS(theta)

    return
  end subroutine ang2vec
  !=======================================================================
  subroutine vec2ang(vector, theta, phi)
    !=======================================================================
    !     renders the angles theta, phi corresponding to vector (x,y,z)
    !     theta (co-latitude measured from North pole, in [0,Pi] radians)
    !     and phi (longitude measured eastward, in [0,2Pi[ radians)
    !     North pole is (x,y,z)=(0,0,1)
    !     added by EH, Feb 2000
    !=======================================================================
    REAL(KIND=DP), INTENT(IN), dimension(1:) :: vector
    REAL(KIND=DP), INTENT(OUT) :: theta, phi

    REAL(KIND=DP) :: dnorm, z
    !=======================================================================

    dnorm = SQRT(vector(1)**2+vector(2)**2+vector(3)**2)

    z = vector(3) / dnorm
    theta = ACOS(z)

    phi = 0.0_dp
    if (vector(1) /= 0.0_dp .or. vector(2) /= 0.0_dp) &
         &     phi = ATAN2(vector(2),vector(1)) ! phi in ]-pi,pi]
    if (phi < 0.0)     phi = phi + twopi ! phi in [0,2pi[

    return
  end subroutine vec2ang
  !=======================================================================
  function nside2npix(nside) result(npix_result)
    !=======================================================================
    ! given nside, returns npix such that npix = 12*nside^2
    !  nside should be a power of 2 smaller than ns_max
    !  if not, -1 is returned
    ! EH, Feb-2000
    ! 2009-03-04: returns i8b result, faster
    !=======================================================================
    INTEGER(KIND=I8B)             :: npix_result
    INTEGER(KIND=I4B), INTENT(IN) :: nside

    INTEGER(KIND=I8B) :: npix
    CHARACTER(LEN=*), PARAMETER :: code = "nside2npix"
    !=======================================================================

    npix = (12_i8b*nside)*nside
    if (nside < 1 .or. nside > ns_max .or. iand(nside-1,nside) /= 0) then
       print*,code//": Nside="//trim(string(nside))//" is not a power of 2."
       npix = -1
    endif
    npix_result = npix

    return
  end function nside2npix
  !=======================================================================
  subroutine surface_triangle(vec1, vec2, vec3, surface)
    !=======================================================================
    ! returns the surface in steradians
    !  of the spherical triangle with vertices vec1, vec2, vec3
    !
    ! algorithm : finds triangle sides and uses l'Huilier formula to compute
    ! "spherical excess" = surface area of triangle on a sphere of radius one
    ! see, eg Bronshtein, Semendyayev Eq 2.86
    !=======================================================================
    real(kind=dp), dimension(1:), intent(in) :: vec1, vec2, vec3
    real(kind=dp), intent(out) :: surface

    !   real(kind=dp), dimension(1:3) :: v1, v2, v3
    real(kind=dp), dimension(1:3) :: side
    !  real(kind=dp) :: hp
    real(kind=dp) :: x0, x1, x2, x3
    !=======================================================================

    ! half perimeter
!   hp = 0.5_dp * (side1 + side2 + side3)

!   ! l'Huilier formula
!   x0 = tan( hp          * 0.5_dp)
!   x1 = tan((hp - side1) * 0.5_dp)
!   x2 = tan((hp - side2) * 0.5_dp)
!   x3 = tan((hp - side3) * 0.5_dp)

    ! find triangle sides
    call angdist(vec2, vec3, side(1))
    call angdist(vec3, vec1, side(2))
    call angdist(vec1, vec2, side(3))
    ! divide by 4
    side(1:3) = side(1:3) * 0.25_dp

    ! l'Huilier formula
    x0 = tan( side(1) + side(2) + side(3) )
    x1 = tan(-side(1) + side(2) + side(3) )
    x2 = tan( side(1) - side(2) + side(3) )
    x3 = tan( side(1) + side(2) - side(3) )
    surface = 4.0_dp * atan( sqrt(x0 * x1 * x2 * x3) )

    return
  end subroutine surface_triangle
  !=======================================================================
  subroutine angdist(v1, v2, dist)
    !=======================================================================
    ! call angdist(v1, v2, dist)
    ! computes the angular distance dist (in rad) between 2 vectors v1 and v2
    ! in general dist = acos ( v1 . v2 )
    ! except if the 2 vectors are almost aligned.
    !=======================================================================
    real(kind=DP), intent(IN), dimension(1:) :: v1, v2
    real(kind=DP), intent(OUT) :: dist

    real(kind=DP), dimension(1:3) :: r1, r2, vdiff
    real(kind=DP) :: diff, sprod
    !=======================================================================

    ! normalize both vectors
    r1(1:3) = v1(1:3) / sqrt(dot_product(v1,v1))
    r2(1:3) = v2(1:3) / sqrt(dot_product(v2,v2))

    sprod = DOT_PRODUCT(r1, r2)

    if (sprod > 0.999_dp) then
       ! almost colinear vectors
       vdiff(1:3) = r1(1:3) - r2(1:3)
       diff = sqrt(dot_product(vdiff,vdiff)) ! norm of difference
       dist = 2.0_dp * asin(diff * 0.5_dp)

    else if (sprod < -0.999_dp) then
       ! almost anti-colinear vectors
       vdiff(1:3) = r1(1:3) + r2(1:3)
       diff = sqrt(dot_product(vdiff,vdiff)) ! norm of sum
       dist = PI - 2.0_dp * asin(diff * 0.5_dp)

    else
       ! other cases
       dist = acos( sprod )
    endif


    return
  end subroutine angdist
  !=======================================================================
  subroutine vect_prod(v1, v2, v3) !
    !=======================================================================
    !     returns in v3 the vectorial product of the 2 3-dimensional
    !     vectors v1 and v2
    !=======================================================================
    real(kind=DP), dimension(1:), INTENT(IN)  :: v1, v2
    real(kind=DP), dimension(1:), INTENT(OUT) :: v3
    !=======================================================================

    v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
    v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
    v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

    return
  end subroutine vect_prod

  !****************************************************************************

  !=======================================================================
  function nside2ntemplates(nside) result(ntemplates)
    !=======================================================================
    ! returns the number of template pixels
    ! 2009-03-24: accepts Nside > 8192, and returns I8B result
    !

    integer(kind=i8b) :: ntemplates
    integer(kind=i4b), intent(IN) :: nside

    ntemplates = 1_I8B + nside * (nside + 6_I8B)
    ntemplates = ntemplates / 4_I8B

    return
  end function nside2ntemplates




!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
  !=======================================================================
  subroutine pix2ang_ring_old(nside, ipix, theta, phi) ! TO BE REMOVED
    !=======================================================================
    !     renders theta and phi coordinates of the nominal pixel center
    !     for the pixel number ipix (RING scheme)
    !     given the map resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: ipix, nside
    REAL(KIND=DP), INTENT(OUT) ::  theta, phi

    INTEGER(KIND=I4B) ::  nl2, nl4, npix, ncap, iring, iphi, ip, ipix1
    REAL(KIND=DP) ::  fodd, hip, fihip
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error ("nside out of range")
    npix = 12*nside**2       ! total number of points
    if (ipix <0 .or. ipix>npix-1) call fatal_error ("ipix out of range")

    ipix1 = ipix + 1 ! in {1, npix}
    nl2 = 2*nside
    ncap = 2*nside*(nside-1) ! points in each polar cap, =0 for nside =1

    if (ipix1 <= ncap) then ! North Polar cap -------------

       hip   = ipix1*0.5_dp
       fihip = AINT ( hip , kind=DP)
       iring = INT( SQRT( hip - SQRT(fihip) ) ) + 1 ! counted from North pole
       iphi  = ipix1 - 2*iring*(iring - 1)

       theta = ACOS( 1.0_dp - iring**2 / (3.0_dp*nside**2) )
       phi   = (real(iphi,kind=dp) - 0.5_dp) * PI/(2.0_dp*iring)

    elseif (ipix1 <= nl2*(5*nside+1)) then ! Equatorial region ------

       ip    = ipix1 - ncap - 1
       nl4   = 4*nside
       iring = INT( ip / nl4 ) + nside ! counted from North pole
       iphi  = MODULO(ip,nl4) + 1

       fodd  = 0.5_dp * (1 + MODULO(iring+nside,2))  ! 1 if iring+nside is odd, 1/2 otherwise
       theta = ACOS( (nl2 - iring) / (1.5_dp*nside) )
       phi   = (real(iphi,kind=dp) - fodd) * PI /(2.0_dp*nside)

    else ! South Polar cap -----------------------------------

       ip    = npix - ipix1 + 1
       hip   = ip*0.5_dp
       fihip = AINT ( hip , kind=DP)
       iring = INT( SQRT( hip - SQRT(fihip) ) ) + 1     ! counted from South pole
       iphi  = 4*iring + 1 - (ip - 2*iring*(iring-1))

       theta = ACOS( -1.0_dp + iring**2 / (3.0_dp*nside**2) )
       phi   = (real(iphi,kind=dp) - 0.5_dp) * PI/(2.0_dp*iring)

    endif

    return
  end subroutine pix2ang_ring_old
  !=======================================================================
  subroutine pix2vec_ring_old(nside, ipix, vector, vertex) ! TO BE REMOVED
    !=======================================================================
    !     renders vector (x,y,z) coordinates of the nominal pixel center
    !     for the pixel number ipix (RING scheme)
    !     given the map resolution parameter nside
    !     also returns the (x,y,z) position of the 4 pixel vertices (=corners)
    !     in the order N,W,S,E
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN)                             :: ipix, nside
    REAL(KIND=DP),     INTENT(OUT),dimension(1:)              :: vector
    REAL(KIND=DP),     INTENT(OUT),dimension(1:,1:), optional :: vertex

    INTEGER(KIND=I4B) :: nl2, nl4, npix, ncap, iring, iphi, ip, ipix1
    REAL(KIND=DP) ::  fact1, fact2, fodd, hip, fihip, z, sth, phi

    real(kind=DP) :: phi_nv, phi_wv, phi_sv, phi_ev
    real(kind=DP) :: z_nv, z_sv, sth_nv, sth_sv
    real(kind=DP) :: hdelta_phi
    integer(kind=I4B) :: iphi_mod, iphi_rat
    logical(kind=LGT) :: do_vertex
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")
    npix = 12*nside**2       ! total number of points
    if (ipix <0 .or. ipix>npix-1) call fatal_error("ipix out of range")

    ipix1 = ipix + 1 ! in {1, npix}
    nl2 = 2*nside
    nl4 = 4*nside
    ncap = 2*nside*(nside-1) ! points in each polar cap, =0 for nside =1
    fact1 = 1.5_dp*nside
    fact2 = 3.0_dp*nside**2

    do_vertex = .false.
    if (present(vertex)) then
       if (size(vertex,dim=1) >= 3 .and. size(vertex,dim=2) >= 4) then
          do_vertex = .true.
       else
          call fatal_error(" pix2vec_ring : vertex array has wrong size ")
       endif
    endif

    phi_nv = 0.0_dp
    phi_sv = 0.0_dp
    if (ipix1 <= ncap) then ! North Polar cap -------------

       hip   = ipix1/2.0_dp
       fihip = AINT ( hip ,kind=DP)
       iring = INT( SQRT( hip - SQRT(fihip) ) ) + 1 ! counted from North pole
       iphi  = ipix1 - 2*iring*(iring - 1)

       z =  1.0_dp - iring**2 / fact2
       phi   = (real(iphi,kind=dp) - 0.5_dp) * PI/(2.0_dp*iring)

       if (do_vertex) then
          hdelta_phi = PI/(4.0_dp*iring)   ! half pixel width
          z_nv = 1.0_dp - (iring-1)**2 / fact2
          z_sv = 1.0_dp - (iring+1)**2 / fact2
          iphi_mod = MODULO(iphi-1, iring) ! in {0,1,... iring-1}
          iphi_rat = (iphi-1) / iring      ! in {0,1,2,3}
          if (iring > 1) phi_nv = HALFPI * (iphi_rat +  iphi_mod   /real(iring-1,kind=dp))
          phi_sv                = HALFPI * (iphi_rat + (iphi_mod+1)/real(iring+1,kind=dp))
       endif


    elseif (ipix1 <= nl2*(5*nside+1)) then ! Equatorial region ------

       ip    = ipix1 - ncap - 1
       iring = INT( ip / nl4 ) + nside ! counted from North pole
       iphi  = MODULO(ip,nl4) + 1

       fodd  = 0.5_dp * (1 + MODULO(iring+nside,2))  ! 1 if iring+nside is odd, 1/2 otherwise
       z = (nl2 - iring) / fact1
       phi   = (real(iphi,kind=dp) - fodd) * PI /(2.0_dp*nside)

       if (do_vertex) then
          hdelta_phi = PI/(4.0_dp*nside)   ! half pixel width
          phi_nv = phi
          phi_sv = phi
          z_nv = (nl2 - iring +1) / fact1
          z_sv = (nl2 - iring -1) / fact1
          if (iring == nside) then ! northern transition
             z_nv = 1.0_dp - (nside-1)**2 / fact2
             iphi_mod = MODULO(iphi-1, nside) ! in {0,1,... nside-1}
             iphi_rat = (iphi-1) / nside      ! in {0,1,2,3}
             if (nside > 1) phi_nv = HALFPI * (iphi_rat +  iphi_mod   /real(nside-1,kind=dp))
          elseif (iring == 3*nside) then ! southern transition
             z_sv = -1.0_dp + (nside-1)**2 / fact2
             iphi_mod = MODULO(iphi-1, nside) ! in {0,1,... iring-1}
             iphi_rat = (iphi-1) / nside      ! in {0,1,2,3}
             if (nside > 1) phi_sv = HALFPI * (iphi_rat +  iphi_mod   /real(nside-1,kind=dp))
          endif
       endif

    else ! South Polar cap -----------------------------------

       ip    = npix - ipix1 + 1
       hip   = ip/2.0_dp
       fihip = AINT ( hip ,kind=DP)
       iring = INT( SQRT( hip - SQRT(fihip) ) ) + 1     ! counted from South pole
       iphi  = 4*iring + 1 - (ip - 2*iring*(iring-1))

       z = -1.0_dp + iring**2 / fact2
       phi   = (real(iphi,kind=dp) - 0.5_dp) * PI/(2.0_dp*iring)

       if (do_vertex) then
          hdelta_phi = PI/(4.0_dp*iring)   ! half pixel width
          z_nv = -1.0_dp + (iring+1)**2 / fact2
          z_sv = -1.0_dp + (iring-1)**2 / fact2
          iphi_mod = MODULO(iphi-1, iring) ! in {0,1,... iring-1}
          iphi_rat = (iphi-1) / iring      ! in {0,1,2,3}
          phi_nv                = HALFPI * (iphi_rat + (iphi_mod+1)/real(iring+1,kind=dp))
          if (iring > 1) phi_sv = HALFPI * (iphi_rat +  iphi_mod   /real(iring-1,kind=dp))
       endif

    endif

    ! pixel center
    sth = SQRT((1.0_dp-z)*(1.0_dp+z))
    vector(1) = sth * COS(phi)
    vector(2) = sth * SIN(phi)
    vector(3) = z

    if (do_vertex) then
       ! west vertex
       phi_wv      = phi - hdelta_phi
       vertex(1,2) = sth * COS(phi_wv)
       vertex(2,2) = sth * SIN(phi_wv)
       vertex(3,2) = z

       ! east vertex
       phi_ev      = phi + hdelta_phi
       vertex(1,4) = sth * COS(phi_ev)
       vertex(2,4) = sth * SIN(phi_ev)
       vertex(3,4) = z

       ! north vertex
       sth_nv = SQRT((1.0_dp-z_nv)*(1.0_dp+z_nv))
       vertex(1,1) = sth_nv * COS(phi_nv)
       vertex(2,1) = sth_nv * SIN(phi_nv)
       vertex(3,1) = z_nv

       ! south vertex
       sth_sv = SQRT((1.0_dp-z_sv)*(1.0_dp+z_sv))
       vertex(1,3) = sth_sv * COS(phi_sv)
       vertex(2,3) = sth_sv * SIN(phi_sv)
       vertex(3,3) = z_sv
    endif


    return
  end subroutine pix2vec_ring_old
  !=======================================================================
  subroutine ang2pix_ring_old(nside, theta, phi, ipix) ! TO BE REMOVED
    !=======================================================================
    !     renders the pixel number ipix (RING scheme) for a pixel which contains
    !     a point on a sphere at coordinates theta and phi, given the map
    !     resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside
    INTEGER(KIND=I4B), INTENT(OUT) :: ipix
    REAL(KIND=DP), INTENT(IN) ::  theta, phi

    INTEGER(KIND=I4B) ::  nl4, jp, jm
    REAL(KIND=DP) ::  z, za, tt, tp, tmp, temp1, temp2
    INTEGER(KIND=I4B) ::  ir, ip, kshift

    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error ("nside out of range")
    if (theta<0.0_dp .or. theta>pi)  then
       print*,"ANG2PIX_RING: theta : ",theta," is out of range [0, Pi]"
       call fatal_error
    endif

    z = COS(theta)
    za = ABS(z)
    tt = MODULO( phi, twopi) / halfpi  ! in [0,4)


    if ( za <= twothird ) then ! Equatorial region ------------------
       temp1 = nside*(.5_dp+tt)
       temp2 = nside*.75_dp*z
       jp = int(temp1-temp2) ! index of  ascending edge line
       jm = int(temp1+temp2) ! index of descending edge line

       ir = nside + 1 + jp - jm ! in {1,2n+1} (ring number counted from z=2/3)
       kshift = 1 - modulo(ir,2) ! kshift=1 if ir even, 0 otherwise

       nl4 = 4*nside
       ip = INT( ( jp+jm - nside + kshift + 1 ) / 2 ) ! in {0,4n-1}
       if (ip >= nl4) ip = ip - nl4

       ipix = 2*nside*(nside-1) + nl4*(ir-1) + ip

    else ! North & South polar caps -----------------------------

       tp = tt - INT(tt)      !MODULO(tt,1.0_dp)
       tmp = nside * SQRT( 3.0_dp*(1.0_dp - za) )

       jp = INT(tp          * tmp ) ! increasing edge line index
       jm = INT((1.0_dp - tp) * tmp ) ! decreasing edge line index

       ir = jp + jm + 1        ! ring number counted from the closest pole
       ip = INT( tt * ir )     ! in {0,4*ir-1}
       if (ip >= 4*ir) ip = ip - 4*ir

       if (z>0._dp) then
          ipix = 2*ir*(ir-1) + ip
       else
          ipix = 12*nside**2 - 2*ir*(ir+1) + ip
       endif

    endif

    return
  end subroutine ang2pix_ring_old
  !=======================================================================
  subroutine vec2pix_ring_old(nside, vector, ipix) ! TO BE REMOVED
    !=======================================================================
    !     renders the pixel number ipix (RING scheme) for a pixel which contains
    !     a point on a sphere at coordinate vector (=x,y,z), given the map
    !     resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside
    REAL(KIND=DP),     INTENT(IN), dimension(1:) :: vector
    INTEGER(KIND=I4B), INTENT(OUT) :: ipix

    INTEGER(KIND=I4B) :: nl2, nl4, ncap, npix, jp, jm, ipix1
    REAL(KIND=DP) ::  z, za, tt, tp, tmp, dnorm, phi
    INTEGER(KIND=I4B) :: ir, ip, kshift

    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")

    dnorm = SQRT(vector(1)**2+vector(2)**2+vector(3)**2)
    z = vector(3) / dnorm
    phi = 0.0_dp
    if (vector(1) /= 0.0_dp .or. vector(2) /= 0.0_dp) &
         &     phi = ATAN2(vector(2),vector(1)) ! phi in ]-pi,pi]

    za = ABS(z)
    if (phi < 0.0)     phi = phi + twopi ! phi in [0,2pi[
    tt = phi / halfpi   ! in [0,4)

    nl2 = 2*nside
    nl4 = 4*nside
    ncap  = nl2*(nside-1) ! number of pixels in the north polar cap
    npix  = 12*nside**2

    if ( za <= twothird ) then ! Equatorial region ------------------

       jp = INT(nside*(0.5_dp + tt - z*0.75_dp)) ! index of  ascending edge line
       jm = INT(nside*(0.5_dp + tt + z*0.75_dp)) ! index of descending edge line

       ir = nside + 1 + jp - jm ! in {1,2n+1} (ring number counted from z=2/3)
       kshift = 0
       if (MODULO(ir,2) == 0) kshift = 1 ! kshift=1 if ir even, 0 otherwise

       ip = INT( ( jp+jm - nside + kshift + 1 ) / 2 ) + 1 ! in {1,4n}
       if (ip > nl4) ip = ip - nl4

       ipix1 = ncap + nl4*(ir-1) + ip

    else ! North & South polar caps -----------------------------

       tp = tt - INT(tt)      !MODULO(tt,1.0_dp)
       tmp = SQRT( 3.0_dp*(1.0_dp - za) )

       jp = INT( nside * tp          * tmp ) ! increasing edge line index
       jm = INT( nside * (1.0_dp - tp) * tmp ) ! decreasing edge line index

       ir = jp + jm + 1        ! ring number counted from the closest pole
       ip = INT( tt * ir ) + 1 ! in {1,4*ir}
       if (ip > 4*ir) ip = ip - 4*ir

       ipix1 = 2*ir*(ir-1) + ip
       if (z <= 0.0_dp) then
          ipix1 = npix - 2*ir*(ir+1) + ip
       endif

    endif

    ipix = ipix1 - 1 ! in {0, npix-1}

    return
  end subroutine vec2pix_ring_old
  !=======================================================================
  subroutine pix2ang_nest_old(nside, ipix, theta, phi) ! TO BE REMOVED
    !=======================================================================
    !     renders theta and phi coordinates of the nominal pixel center
    !     for the pixel number ipix (NESTED scheme)
    !     given the map resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside, ipix
    REAL(KIND=DP), INTENT(OUT) :: theta, phi

    INTEGER(KIND=I4B) :: npix, npface, &
         &     ipf, ip_low, ip_trunc, ip_med, ip_hi, &
         &     jrt, jr, nr, jpt, jp, kshift, nl4
    REAL(KIND=DP) :: z, fn, fact1, fact2

    INTEGER(KIND=I4B) :: ix, iy, face_num
!     common /xy_nest/ ix, iy, face_num ! can be useful to calling routine

    ! coordinate of the lowest corner of each face
    INTEGER(KIND=I4B), dimension(1:12) :: jrll = (/ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 /) ! in unit of nside
    INTEGER(KIND=I4B), dimension(1:12) :: jpll = (/ 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 /) ! in unit of nside/2
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")
    npix = 12 * nside**2
    if (ipix <0 .or. ipix>npix-1) call fatal_error("ipix out of range")

    !     initiates the array for the pixel number -> (x,y) mapping
    if (pix2x(1023) <= 0) call mk_pix2xy()

    fn = real(nside,kind=dp)
    fact1 = 1.0_dp/(3.0_dp*fn*fn)
    fact2 = 2.0_dp/(3.0_dp*fn)
    nl4   = 4*nside

    !     finds the face, and the number in the face
    npface = nside**2

    face_num = ipix/npface  ! face number in {0,11}
    ipf = MODULO(ipix,npface)  ! pixel number in the face {0,npface-1}

    !     finds the x,y on the face (starting from the lowest corner)
    !     from the pixel number
    ip_low = MODULO(ipf,1024)       ! content of the last 10 bits
    ip_trunc =   ipf/1024        ! truncation of the last 10 bits
    ip_med = MODULO(ip_trunc,1024)  ! content of the next 10 bits
    ip_hi  =     ip_trunc/1024   ! content of the high weight 10 bits

    ix = 1024*pix2x(ip_hi) + 32*pix2x(ip_med) + pix2x(ip_low)
    iy = 1024*pix2y(ip_hi) + 32*pix2y(ip_med) + pix2y(ip_low)

    !     transforms this in (horizontal, vertical) coordinates
    jrt = ix + iy  ! 'vertical' in {0,2*(nside-1)}
    jpt = ix - iy  ! 'horizontal' in {-nside+1,nside-1}

    !     computes the z coordinate on the sphere
    jr =  jrll(face_num+1)*nside - jrt - 1   ! ring number in {1,4*nside-1}

    nr = nside                  ! equatorial region (the most frequent)
    z  = (2*nside-jr)*fact2
    kshift = MODULO(jr - nside, 2)
    if (jr < nside) then     ! north pole region
       nr = jr
       z = 1.0_dp - nr*nr*fact1
       kshift = 0
    else if (jr > 3*nside) then ! south pole region
       nr = nl4 - jr
       z = - 1.0_dp + nr*nr*fact1
       kshift = 0
    endif
    theta = ACOS(z)

    !     computes the phi coordinate on the sphere, in [0,2Pi]
    jp = (jpll(face_num+1)*nr + jpt + 1 + kshift)/2  ! 'phi' number in the ring in {1,4*nr}
    if (jp > nl4) jp = jp - nl4
    if (jp < 1)   jp = jp + nl4

    phi = (jp - (kshift+1)*0.5_dp) * (halfpi / nr)

    return
  end subroutine pix2ang_nest_old
  !=======================================================================
  subroutine pix2vec_nest_old(nside, ipix, vector, vertex) ! TO BE REMOVED
    !=======================================================================
    !     renders vector (x,y,z) coordinates of the nominal pixel center
    !     for the pixel number ipix (NESTED scheme)
    !     given the map resolution parameter nside
    !     also returns the (x,y,z) position of the 4 pixel vertices (=corners)
    !     in the order N,W,S,E
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside, ipix
    REAL(KIND=DP), INTENT(OUT), dimension(1:) :: vector
    REAL(KIND=DP),     INTENT(OUT),dimension(1:,1:), optional :: vertex

    INTEGER(KIND=I4B) :: npix, npface, &
         &     ipf, ip_low, ip_trunc, ip_med, ip_hi, &
         &     jrt, jr, nr, jpt, jp, kshift, nl4
    REAL(KIND=DP) :: z, fn, fact1, fact2, sth, phi

    INTEGER(KIND=I4B) ::  ix, iy, face_num
!     common /xy_nest/ ix, iy, face_num ! can be useful to calling routine

    ! coordinate of the lowest corner of each face
    INTEGER(KIND=I4B), dimension(1:12) :: jrll = (/ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 /) ! in unit of nside
    INTEGER(KIND=I4B), dimension(1:12) :: jpll = (/ 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 /) ! in unit of nside/2

    real(kind=DP) :: phi_nv, phi_wv, phi_sv, phi_ev, phi_up, phi_dn
    real(kind=DP) :: z_nv, z_sv, sth_nv, sth_sv
    real(kind=DP) :: hdelta_phi
    integer(kind=I4B) :: iphi_mod, iphi_rat
    logical(kind=LGT) :: do_vertex
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")
    npix = 12 * nside**2
    if (ipix <0 .or. ipix>npix-1) call fatal_error("ipix out of range")

    !     initiates the array for the pixel number -> (x,y) mapping
    if (pix2x(1023) <= 0) call mk_pix2xy()

    fn = real(nside,kind=dp)
    fact1 = 1.0_dp/(3.0_dp*fn*fn)
    fact2 = 2.0_dp/(3.0_dp*fn)
    nl4   = 4*nside

    do_vertex = .false.
    if (present(vertex)) then
       if (size(vertex,dim=1) >= 3 .and. size(vertex,dim=2) >= 4) then
          do_vertex = .true.
       else
          call fatal_error(" pix2vec_ring : vertex array has wrong size ")
       endif
    endif

    !     finds the face, and the number in the face
    npface = nside**2

    face_num = ipix/npface  ! face number in {0,11}
    ipf = MODULO(ipix,npface)  ! pixel number in the face {0,npface-1}

    !     finds the x,y on the face (starting from the lowest corner)
    !     from the pixel number
    ip_low = MODULO(ipf,1024)       ! content of the last 10 bits
    ip_trunc =   ipf/1024        ! truncation of the last 10 bits
    ip_med = MODULO(ip_trunc,1024)  ! content of the next 10 bits
    ip_hi  =     ip_trunc/1024   ! content of the high weight 10 bits

    ix = 1024*pix2x(ip_hi) + 32*pix2x(ip_med) + pix2x(ip_low)
    iy = 1024*pix2y(ip_hi) + 32*pix2y(ip_med) + pix2y(ip_low)

    !     transforms this in (horizontal, vertical) coordinates
    jrt = ix + iy  ! 'vertical' in {0,2*(nside-1)}
    jpt = ix - iy  ! 'horizontal' in {-nside+1,nside-1}

    !     computes the z coordinate on the sphere
    jr =  jrll(face_num+1)*nside - jrt - 1   ! ring number in {1,4*nside-1}

    nr = nside                  ! equatorial region (the most frequent)
    z  = (2*nside-jr)*fact2
    kshift = MODULO(jr - nside, 2)
    if (do_vertex) then
       z_nv = (2*nside-jr+1)*fact2
       z_sv = (2*nside-jr-1)*fact2
       if (jr == nside) then ! northern transition
          z_nv =  1.0_dp - (nside-1)**2 * fact1
       elseif (jr == 3*nside) then  ! southern transition
          z_sv = -1.0_dp + (nside-1)**2 * fact1
       endif
    endif
    if (jr < nside) then     ! north pole region
       nr = jr
       z = 1.0_dp - nr*nr*fact1
       kshift = 0
       if (do_vertex) then
          z_nv = 1.0_dp - (nr-1)**2*fact1
          z_sv = 1.0_dp - (nr+1)**2*fact1
       endif
    else if (jr > 3*nside) then ! south pole region
       nr = nl4 - jr
       z = - 1.0_dp + nr*nr*fact1
       kshift = 0
       if (do_vertex) then
          z_nv = - 1.0_dp + (nr+1)**2*fact1
          z_sv = - 1.0_dp + (nr-1)**2*fact1
       endif
    endif

    !     computes the phi coordinate on the sphere, in [0,2Pi]
    jp = (jpll(face_num+1)*nr + jpt + 1 + kshift)/2  ! 'phi' number in the ring in {1,4*nr}
    if (jp > nl4) jp = jp - nl4
    if (jp < 1)   jp = jp + nl4

    phi = (jp - (kshift+1)*0.5_dp) * (halfpi / nr)

    sth = SQRT((1.0_dp-z)*(1.0_dp+z))
    vector(1) = sth * COS(phi)
    vector(2) = sth * SIN(phi)
    vector(3) = z

    if (do_vertex) then
       phi_nv = phi
       phi_sv = phi

       phi_up = 0.0_dp
       iphi_mod = MODULO(jp-1, nr) ! in {0,1,... nr-1}
       iphi_rat = (jp-1) / nr      ! in {0,1,2,3}
       if (nr > 1) phi_up = HALFPI * (iphi_rat +  iphi_mod   /real(nr-1,kind=dp))
       phi_dn             = HALFPI * (iphi_rat + (iphi_mod+1)/real(nr+1,kind=dp))
       if (jr < nside) then            ! North polar cap
          phi_nv = phi_up
          phi_sv = phi_dn
       else if (jr > 3*nside) then     ! South polar cap
          phi_nv = phi_dn
          phi_sv = phi_up
       else if (jr == nside) then      ! North transition
          phi_nv = phi_up
       else if (jr == 3*nside) then    ! South transition
          phi_sv = phi_up
       endif

       hdelta_phi = PI / (4.0_dp*nr)

       ! west vertex
       phi_wv      = phi - hdelta_phi
       vertex(1,2) = sth * COS(phi_wv)
       vertex(2,2) = sth * SIN(phi_wv)
       vertex(3,2) = z

       ! east vertex
       phi_ev      = phi + hdelta_phi
       vertex(1,4) = sth * COS(phi_ev)
       vertex(2,4) = sth * SIN(phi_ev)
       vertex(3,4) = z

       ! north vertex
       sth_nv = SQRT((1.0_dp-z_nv)*(1.0_dp+z_nv))
       vertex(1,1) = sth_nv * COS(phi_nv)
       vertex(2,1) = sth_nv * SIN(phi_nv)
       vertex(3,1) = z_nv

       ! south vertex
       sth_sv = SQRT((1.0_dp-z_sv)*(1.0_dp+z_sv))
       vertex(1,3) = sth_sv * COS(phi_sv)
       vertex(2,3) = sth_sv * SIN(phi_sv)
       vertex(3,3) = z_sv
    endif

    return
  end subroutine pix2vec_nest_old
  !=======================================================================
  subroutine ang2pix_nest_old(nside, theta, phi, ipix) ! TO BE REMOVED
    !=======================================================================
    !     renders the pixel number ipix (NESTED scheme) for a pixel which contains
    !     a point on a sphere at coordinates theta and phi, given the map
    !     resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside
    INTEGER(KIND=I4B), INTENT(OUT) :: ipix
    REAL(KIND=DP), INTENT(IN) ::  theta, phi

    REAL(KIND=DP) ::  z, za, tt, tp, tmp
    INTEGER(KIND=I4B) :: jp, jm, ifp, ifm, face_num, &
         &     ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf, ntt

    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max4) call fatal_error("nside out of range")
    if (theta<0.0_dp .or. theta>pi)  then
       print*,"ANG2PIX_NEST: theta : ",theta," is out of range [0,Pi]"
       call fatal_error
    endif
    if (x2pix(128) <= 0) call mk_xy2pix()

    z  = COS(theta)
    za = ABS(z)
    tt = MODULO(phi, twopi) / halfpi  ! in [0,4[

    if (za <= twothird) then ! equatorial region

       !        (the index of edge lines increase when the longitude=phi goes up)
       jp = INT(ns_max4*(0.5_dp + tt - z*0.75_dp)) !  ascending edge line index
       jm = INT(ns_max4*(0.5_dp + tt + z*0.75_dp)) ! descending edge line index

       !        finds the face
       ifp = jp / ns_max4  ! in {0,4}
       ifm = jm / ns_max4
       if (ifp == ifm) then          ! faces 4 to 7
          face_num = MODULO(ifp,4) + 4
       else if (ifp < ifm) then     ! (half-)faces 0 to 3
          face_num = MODULO(ifp,4)
       else                            ! (half-)faces 8 to 11
          face_num = MODULO(ifm,4) + 8
       endif

       ix = MODULO(jm, ns_max4)
       iy = ns_max4 - MODULO(jp, ns_max4) - 1

    else ! polar region, za > 2/3

       ntt = INT(tt)
       if (ntt >= 4) ntt = 3
       tp = tt - ntt
       tmp = SQRT( 3.0_dp*(1.0_dp - za) )  ! in ]0,1]

       !        (the index of edge lines increase when distance from the closest pole goes up)
       jp = INT( ns_max4 * tp          * tmp ) ! line going toward the pole as phi increases
       jm = INT( ns_max4 * (1.0_dp - tp) * tmp ) ! that one goes away of the closest pole
       jp = MIN(ns_max4-1, jp) ! for points too close to the boundary
       jm = MIN(ns_max4-1, jm)

       !        finds the face and pixel's (x,y)
       if (z >= 0) then
          face_num = ntt  ! in {0,3}
          ix = ns_max4 - jm - 1
          iy = ns_max4 - jp - 1
       else
          face_num = ntt + 8 ! in {8,11}
          ix =  jp
          iy =  jm
       endif

       !         print*,z,face_num,ix,iy
    endif

    ix_low = MODULO(ix,128)
    ix_hi  =     ix/128
    iy_low = MODULO(iy,128)
    iy_hi  =     iy/128

    ipf =  (x2pix(ix_hi +1)+y2pix(iy_hi +1)) * (128 * 128) &
         &     + (x2pix(ix_low+1)+y2pix(iy_low+1))

    ipf = ipf / ( ns_max4/nside ) **2  ! in {0, nside**2 - 1}

    ipix = ipf + face_num* nside **2    ! in {0, 12*nside**2 - 1}

    return
  end subroutine ang2pix_nest_old
  !=======================================================================
  subroutine vec2pix_nest_old(nside, vector, ipix) ! TO BE REMOVED
    !=======================================================================
    !     renders the pixel number ipix (NESTED scheme) for a pixel which contains
    !     a point on a sphere at coordinate vector (=x,y,z), given the map
    !     resolution parameter nside
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside
    INTEGER(KIND=I4B), INTENT(OUT) :: ipix
    REAL(KIND=DP), INTENT(IN), dimension(1:) ::  vector

    REAL(KIND=DP) ::  z, za, tt, tp, tmp, dnorm, phi
    INTEGER(KIND=I4B) ::  jp, jm, ifp, ifm, face_num, &
         &     ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf, ntt

    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max4) call fatal_error("nside out of range")
    if (x2pix(128) <= 0) call mk_xy2pix()

    dnorm = SQRT(vector(1)**2+vector(2)**2+vector(3)**2)
    z = vector(3) / dnorm
    phi = 0.0_dp
    if (vector(1) /= 0.0_dp .or. vector(2) /= 0.0_dp) &
         &     phi = ATAN2(vector(2),vector(1)) ! phi in ]-pi,pi]

    za = ABS(z)
    if (phi < 0.0)    phi = phi + twopi ! phi in [0,2pi[
    tt = phi / halfpi ! in [0,4[

    if (za <= twothird) then ! equatorial region

       !        (the index of edge lines increase when the longitude=phi goes up)
       jp = INT(ns_max4*(0.5_dp + tt - z*0.75_dp)) !  ascending edge line index
       jm = INT(ns_max4*(0.5_dp + tt + z*0.75_dp)) ! descending edge line index

       !        finds the face
       ifp = jp / ns_max4  ! in {0,4}
       ifm = jm / ns_max4
       if (ifp == ifm) then          ! faces 4 to 7
          face_num = MODULO(ifp,4) + 4
       else if (ifp < ifm) then     ! (half-)faces 0 to 3
          face_num = MODULO(ifp,4)
       else                            ! (half-)faces 8 to 11
          face_num = MODULO(ifm,4) + 8
       endif

       ix = MODULO(jm, ns_max4)
       iy = ns_max4 - MODULO(jp, ns_max4) - 1

    else ! polar region, za > 2/3

       ntt = INT(tt)
       if (ntt >= 4) ntt = 3
       tp = tt - ntt
       tmp = SQRT( 3.0_dp*(1.0_dp - za) )  ! in ]0,1]

       !        (the index of edge lines increase when distance from the closest pole goes up)
       jp = INT( ns_max4 * tp          * tmp ) ! line going toward the pole as phi increases
       jm = INT( ns_max4 * (1.0_dp - tp) * tmp ) ! that one goes away of the closest pole
       jp = MIN(ns_max4-1, jp) ! for points too close to the boundary
       jm = MIN(ns_max4-1, jm)

       !        finds the face and pixel's (x,y)
       if (z >= 0) then
          face_num = ntt  ! in {0,3}
          ix = ns_max4 - jm - 1
          iy = ns_max4 - jp - 1
       else
          face_num = ntt + 8 ! in {8,11}
          ix =  jp
          iy =  jm
       endif

       !         print*,z,face_num,ix,iy
    endif

    ix_low = MODULO(ix,128)
    ix_hi  =     ix/128
    iy_low = MODULO(iy,128)
    iy_hi  =     iy/128

    ipf =  (x2pix(ix_hi +1)+y2pix(iy_hi +1)) * (128 * 128) &
         &     + (x2pix(ix_low+1)+y2pix(iy_low+1))

    ipf = ipf / ( ns_max4/nside ) **2  ! in {0, nside**2 - 1}

    ipix = ipf + face_num* nside **2    ! in {0, 12*nside**2 - 1}

    return
  end subroutine vec2pix_nest_old
  !=======================================================================
  subroutine nest2ring_old(nside, ipnest, ipring) ! TO BE REMOVED
    !=======================================================================
    !     performs conversion from NESTED to RING pixel number
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) ::  nside, ipnest
    INTEGER(KIND=I4B), INTENT(OUT) :: ipring
    INTEGER(KIND=I4B) ::  npix, npface, face_num, ncap, n_before, &
         &     ipf, ip_low, ip_trunc, ip_med, ip_hi, &
         &     ix, iy, jrt, jr, nr, jpt, jp, kshift, nl4

    ! coordinate of the lowest corner of each face
    INTEGER(KIND=I4B), dimension(1:12) :: jrll = (/ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 /) ! in unit of nside
    INTEGER(KIND=I4B), dimension(1:12) :: jpll = (/ 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 /) ! in unit of nside/2
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")
    npix = 12 * nside**2
    if (ipnest<0 .or. ipnest>npix-1) call fatal_error("ipnest out of range")

    !     initiates the array for the pixel number -> (x,y) mapping
    if (pix2x(1023) <= 0) call mk_pix2xy()

    ncap  = 2*nside*(nside-1) ! number of points in the North Polar cap
    nl4   = 4*nside

    !     finds the face, and the number in the face
    npface = nside**2
    !ccccc      ip = ipnest - 1         ! in {0,npix-1}

    face_num = ipnest/npface  ! face number in {0,11}
    ipf = MODULO(ipnest,npface)  ! pixel number in the face {0,npface-1}

    !     finds the x,y on the face (starting from the lowest corner)
    !     from the pixel number
    ip_low = MODULO(ipf,1024)       ! content of the last 10 bits
    ip_trunc =   ipf/1024        ! truncation of the last 10 bits
    ip_med = MODULO(ip_trunc,1024)  ! content of the next 10 bits
    ip_hi  =     ip_trunc/1024   ! content of the high weight 10 bits

    ix = 1024*pix2x(ip_hi) + 32*pix2x(ip_med) + pix2x(ip_low)
    iy = 1024*pix2y(ip_hi) + 32*pix2y(ip_med) + pix2y(ip_low)

    !     transforms this in (horizontal, vertical) coordinates
    jrt = ix + iy  ! 'vertical' in {0,2*(nside-1)}
    jpt = ix - iy  ! 'horizontal' in {-nside+1,nside-1}

    !     computes the z coordinate on the sphere
    jr =  jrll(face_num+1)*nside - jrt - 1   ! ring number in {1,4*nside-1}

    nr = nside                  ! equatorial region (the most frequent)
    n_before = ncap + nl4 * (jr - nside)
    kshift = MODULO(jr - nside, 2)
    if (jr < nside) then     ! north pole region
       nr = jr
       n_before = 2 * nr * (nr - 1)
       kshift = 0
    else if (jr > 3*nside) then ! south pole region
       nr = nl4 - jr
       n_before = npix - 2 * (nr + 1) * nr
       kshift = 0
    endif

    !     computes the phi coordinate on the sphere, in [0,2Pi]
    jp = (jpll(face_num+1)*nr + jpt + 1 + kshift)/2  ! 'phi' number in the ring in {1,4*nr}

    if (jp > nl4) jp = jp - nl4
    if (jp < 1)   jp = jp + nl4

    ipring = n_before + jp - 1 ! in {0, npix-1}
    return
  end subroutine nest2ring_old
  !=======================================================================
  subroutine ring2nest_old(nside, ipring, ipnest) ! TO BE REMOVED
    !=======================================================================
    !     performs conversion from RING to NESTED pixel number
    !=======================================================================
    INTEGER(KIND=I4B), INTENT(IN) :: nside, ipring
    INTEGER(KIND=I4B), INTENT(OUT) :: ipnest

    REAL(KIND=DP) :: fihip, hip
    INTEGER(KIND=I4B) :: npix, nl2, nl4, ncap, ip, iphi, ipt, ipring1, &
         &     kshift, face_num, nr, &
         &     irn, ire, irm, irs, irt, ifm , ifp, &
         &     ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf

    ! coordinate of the lowest corner of each face
    INTEGER(KIND=I4B), dimension(1:12) :: jrll = (/ 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 /) ! in unit of nside
    INTEGER(KIND=I4B), dimension(1:12) :: jpll = (/ 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 /) ! in unit of nside/2
    !-----------------------------------------------------------------------
    if (nside<1 .or. nside>ns_max) call fatal_error("nside out of range")
    npix = 12*nside**2      ! total number of points
    if (ipring <0 .or. ipring>npix-1) call fatal_error("ipring out of range")
    if (x2pix(128) <= 0) call mk_xy2pix()

    nl2 = 2*nside
    nl4 = 4*nside
    ncap = nl2*(nside-1) ! points in each polar cap, =0 for nside =1
    ipring1 = ipring + 1

    !     finds the ring number, the position of the ring and the face number
    if (ipring1 <= ncap) then ! north polar cap

       hip   = ipring1/2.0_dp
       fihip = AINT ( hip ,kind=DP)
       irn   = INT( SQRT( hip - SQRT(fihip) ) ) + 1 ! counted from North pole
       iphi  = ipring1 - 2*irn*(irn - 1)

       kshift = 0
       nr = irn                  ! 1/4 of the number of points on the current ring
       face_num = (iphi-1) / irn ! in {0,3}

    elseif (ipring1 <= nl2*(5*nside+1)) then ! equatorial region

       ip    = ipring1 - ncap - 1
       irn   = INT( ip / nl4 ) + nside               ! counted from North pole
       iphi  = MODULO(ip,nl4) + 1

       kshift  = MODULO(irn+nside,2)  ! 1 if irn+nside is odd, 0 otherwise
       nr = nside
       ire =  irn - nside + 1 ! in {1, 2*nside +1}
       irm =  nl2 + 2 - ire
       ifm = (iphi - ire/2 + nside -1) / nside ! face boundary
       ifp = (iphi - irm/2 + nside -1) / nside
       if (ifp == ifm) then          ! faces 4 to 7
          face_num = MODULO(ifp,4) + 4
       else if (ifp + 1 == ifm) then ! (half-)faces 0 to 3
          face_num = ifp
       else if (ifp - 1 == ifm) then ! (half-)faces 8 to 11
          face_num = ifp + 7
       endif

    else ! south polar cap

       ip    = npix - ipring1 + 1
       hip   = ip/2.0_dp
       fihip = AINT ( hip ,kind=DP)
       irs   = INT( SQRT( hip - SQRT(fihip) ) ) + 1  ! counted from South pole
       iphi  = 4*irs + 1 - (ip - 2*irs*(irs-1))

       kshift = 0
       nr = irs
       irn   = nl4 - irs
       face_num = (iphi-1) / irs + 8 ! in {8,11}

    endif

    !     finds the (x,y) on the face
    irt =   irn  - jrll(face_num+1)*nside + 1       ! in {-nside+1,0}
    ipt = 2*iphi - jpll(face_num+1)*nr - kshift - 1 ! in {-nside+1,nside-1}
    if (ipt >= nl2) ipt = ipt - 8*nside ! for the face #4

    ix =  (ipt - irt ) / 2
    iy = -(ipt + irt ) / 2

    ix_low = MODULO(ix,128)
    ix_hi  = ix/128
    iy_low = MODULO(iy,128)
    iy_hi  = iy/128

    ipf =  (x2pix(ix_hi +1)+y2pix(iy_hi +1)) * (128 * 128) &
         &     + (x2pix(ix_low+1)+y2pix(iy_low+1))        ! in {0, nside**2 - 1}


    ipnest = ipf + face_num* nside **2    ! in {0, 12*nside**2 - 1}

    return
  end subroutine ring2nest_old


    !=======================================================================
  subroutine template_pixel_ring_old(nside, pixel, templ8, reflexion) ! TO BE REMOVED
    !=======================================================================
    !    Returns template-pixel index corresponding to each (RING ordered) pixel provided
    !    The reflexion (E-W, or N-W or both) to apply to the pixel to match the
    !    template is optionally returned
    !-------------------------------------------------------------
    !   For a given resolution Nside, there are 12*Nside*Nside pixels on the
    !   sphere, whose shape belong to one of the (Nside+6)*Nside/4 templates.
    !   (2 in the case Nside=1).
    !   Any pixel can be matched to a single of these templates by a combination of
    !    - a rotation around the polar axis with 
    !    - reflexion(s) around a meridian and/or the equator.
    !   The reflexion number returned by this routine for each pixel is
    !   Reflexion         Operation to do to match template
    !     0                 rotation only
    !     1                 rotation + East-West swap
    !     2                 rotation + North-South swap
    !     3                 rotation + East-West + North-South swaps
    !   
    !   The template pixels are all located in the Northern Hemisphere, or on the
    !   Equator.
    !   They are chosen to have
    !       z >= 2/3,      0< phi <= Pi/2               [Nside*(Nside+2)/4]
    !       2/3 > z >= 0,  phi=0, or phi=Pi/(4*Nside)   [Nside]
    !   They are numbered from 0, starting at the North Pole, with the index
    !   increasing in phi, and then increasing for decreasing z.
    !-------------------------------------------------------------
    character(len=*),           parameter :: code = "Template_pixel_ring_old"
    integer(I4B),    intent(IN)           :: nside
    integer(I4B),    intent(IN)           :: pixel
    integer(I4B),    intent(OUT)          :: templ8
    integer(I4B),    intent(OUT), optional:: reflexion
    ! local variable
    integer(I4B) :: npix, ncap, ns4, n0
    integer(I4B) :: ip, iring, ifi, ifd, iff
    integer(I4B) :: refl
    real(DP)     :: fip
    !-------------------------------------------------------------

    npix = nside2npix(nside)
    if (npix < 0) then
       print*,'Invalid Nside = ',nside
       call fatal_error(code//' Abort')
    endif

    if (pixel < 0 .or. pixel >= npix) then
       print*,'Invalid Pixel = ',pixel
       call fatal_error(code//' Abort')
    endif

    ncap = 2*nside*(nside+1)
    ns4  = 4*nside
!     n0   = max( (nside*(nside+2))/4, 1)
    n0   = (1 + nside * (nside + 2)) / 4


    refl = 0 ! no swap

    ! North polar cap
    if (pixel < ncap) then
       ip    = pixel + 1
       fip   = real(int(ip/2) , kind=dp)
       iring = int(sqrt(ip/2.0_dp - sqrt(fip) )) + 1 ! counted from NORTH pole, starting at 1
       ifi   = MOD( (ip - 1 - 2*iring*(iring-1)), iring) ! in [0,iring-1], upwards
       ifd   = iring - 1 - ifi                           ! in [0,iring-1], downwards
       iff   = min(ifi, ifd)                             ! in [0,(iring-1)/2]
       templ8 = (iring*iring)/4 + iff
       if (ifd < ifi) refl = 1 ! East-West swap


    ! North-Equatorial region
    elseif (pixel < (npix+ns4)/2) then
       templ8 = n0 + (pixel - ncap)/ns4

    ! South-Equatorial region
    elseif (pixel < (npix-ncap)) then
       templ8 = n0 + (npix - 1 - pixel - ncap)/ns4
       refl = 2  ! North-South swap

    ! South polar cap
    else
       ip =  npix - pixel
       fip   = real(int(ip/2) , kind=dp)
       iring = int(sqrt( ip/2.0_dp - sqrt(fip) )) + 1 ! counted from SOUTH pole, starting at 1
       ifi   = MOD( (2*iring*(iring+1) - ip), iring)    ! in [0,iring-1], upwards
       ifd  = iring - 1 - ifi                           ! in [0,iring-1], downwards
       iff  = min(ifi, ifd)                             ! in [0,(iring-1)/2]
       templ8 = (iring*iring)/4 + iff
       if (ifd < ifi) refl = 1 ! East-West swap
       refl = refl + 2    ! North-South swap
    endif

    if (present(reflexion)) reflexion = refl

    return
  end subroutine template_pixel_ring_old


  !=======================================================================
  subroutine same_shape_pixels_ring_old(nside, templ8, list, reflexion, nrep) ! TO BE REMOVED
    !   Returns the list of RING ordered pixels having the same shape as the
    !   template provided
    !-------------------------------------------------------------

    character(len=*), parameter :: code = "same_shape_pixels_ring_old"
    integer(I4B),    intent(IN)                        :: nside
    integer(I4B),    intent(IN)                        :: templ8
    integer(I4B),    pointer,   dimension(:), optional :: list
    integer(I4B),    pointer,   dimension(:), optional :: reflexion
    integer(I4B),    intent(OUT)            , optional :: nrep

    integer(i4b) :: npix, ntplt, ncap, ns4, n0
    integer(i4b) :: i, in0, is0, iring, ifi
    integer(i4b), dimension(0:3) :: ramp4 = (/ 0, 1, 2, 3 /)
    integer(i4b), dimension(0:7) :: alt8  = (/ 0, 1, 0, 1, 0, 1, 0, 1 /)
    integer(i4b), dimension(0:7) :: rr8
    logical(lgt) :: do_list, do_rfx, do_nrep
    integer(i4b) :: nrep_in
    integer(i4b) :: ll=1, lr=1
    !-------------------------------------------------------------

    do_list = (present(list))
    do_rfx  = (present(reflexion))
    do_nrep = (present(nrep))

    if (do_rfx .and. .not. do_list) then
       print*,'Error in '//code//'. Can not have Reflexion without pixel list'
       call fatal_error()
    endif

    npix = nside2npix(nside)
    if (npix < 0) then
       print*,'Invalid Nside = ',nside
       call fatal_error(code//' Abort')
    endif

    ntplt= nside2ntemplates(nside)

    if (templ8 < 0 .or. templ8 >= ntplt) then
       print*,'Error on template argument'
       print*,'Nside = ',nside,', Template = ',templ8
       print*,'Template should be in [0, (1+Nside*(Nside+6))/4-1=',ntplt-1,']'
       call fatal_error(code//' Abort')
    endif

    ncap = 2*nside*(nside+1)
    ns4  = 4*nside
!    n0   = max( (nside*(nside+2))/4, 1)
    n0 = (1 + nside * (nside + 2))/ 4

    ! find number of repetition of template around the sphere
    if (templ8 >= n0) then
       nrep_in = ns4 * 2 ! Non- Equator: nrep = 8*Nside
       if (templ8 == ntplt - 1) nrep_in = ns4 ! Equator : nrep = 4*Nside
    else
       nrep_in  = 16
       iring = int(sqrt(4*templ8+1.0_dp)) ! in [1,Nside]
       ifi   = templ8 - (iring*iring)/4
       if (2*ifi+1 == iring) nrep_in = 8
    endif
    if (do_nrep) nrep = nrep_in
          

    ! create or expand (or leave alone) list array
    if (do_list) then
       if (associated(list)) then
          ll = lbound(list,dim=1)
          if (size(list) < nrep_in) then
             deallocate(list)
             allocate(list(ll:ll+nrep_in-1))
          endif
       else
          allocate(list(ll:ll+nrep_in-1))
       endif
       list = -1 ! default value for unused array elements
    endif

    ! create or expand (or leave alone) reflexion array
    if (do_rfx) then
       if (associated(reflexion)) then
          lr = lbound(reflexion,dim=1)
          if (size(reflexion) < nrep_in) then
             deallocate(reflexion)
             allocate(reflexion(lr:lr+nrep_in-1))
          endif
       else
          allocate(reflexion(lr:lr+nrep_in-1))
       endif
       reflexion = -1 ! default value for unused array elements
    endif


    ! fill up list and reflexion arrays
    if (templ8 >= n0) then 
       ! Non Polar pixels (most frequent)
    
       in0 = ncap + (templ8 - n0)*ns4

       if (templ8 == ntplt-1) then 
          ! Equator : nrep = 4*Nside
          if (do_list) list      = in0 + (/ (i, i=0, ns4-1) /)
          if (do_rfx)  reflexion = 0
       else 
          ! Non- Equator: nrep = 8*Nside
          is0 = npix - in0 - ns4
          if (do_list) then
             list(ll    :ll+  ns4-1) = in0 + (/ (i, i=0, ns4-1) /)
             list(ll+ns4:ll+2*ns4-1) = is0 + (/ (i, i=0, ns4-1) /)
          endif
          if (do_rfx) then
             reflexion(lr    :lr+  ns4-1) = 0
             reflexion(lr+ns4:lr+2*ns4-1) = 2
          endif
       endif

    else 
       ! Polar pixels
       in0   = 2*iring*(iring-1)
       is0   = npix - in0 - 4*iring

       if (2*ifi+1 == iring) then 
          ! pixel on phi = Pi/4, Nrep = 8
          if (do_list) then
             list(ll  :ll+3) = in0 + ifi + ramp4*iring
             list(ll+4:ll+7) = is0 + ifi + ramp4*iring
          endif
          if (do_rfx) then
             reflexion = (/ 0, 0, 0, 0, 2, 2, 2, 2 /)
          endif
       else 
          ! Nrep = 16
          rr8(0:6:2) = (/ 0, 1, 2, 3 /) * iring + ifi
          rr8(1:7:2) = (/ 1, 2, 3, 4 /) * iring - ifi - 1
          if (do_list) then
             list(ll  :ll+7 ) = in0 + rr8
             list(ll+8:ll+15) = is0 + rr8
          endif
          if (do_rfx) then
             reflexion(lr  :lr+7 ) = alt8
             reflexion(lr+8:lr+15) = alt8 + 2
          endif
       endif
    endif

    return

  end subroutine same_shape_pixels_ring_old

  !=======================================================================
  subroutine template_pixel_nest_old(nside, pixel, templ8, reflexion) ! TO BE REMOVED
    !=======================================================================
    !    Returns template-pixel index corresponding to each (NESTED ordered) pixel provided
    !    The reflexion (E-W, or N-W or both) to apply to the pixel to match the
    !    template is optionally returned
    !-------------------------------------------------------------
    character(len=*), parameter :: code = "Template_pixel_nest_old"
    integer(I4B),    intent(IN)           :: nside
    integer(I4B),    intent(IN)           :: pixel
    integer(I4B),    intent(OUT)          :: templ8
    integer(I4B),    intent(OUT), optional:: reflexion
    !
    integer(I4B) :: pix_ring, npix
    
    npix = nside2npix(nside)
    if (npix < 0) then
       print*,'Invalid Nside = ',nside
       call fatal_error(code//' Abort')
    endif

    if (pixel < 0 .or. pixel >= npix) then
       print*,'Invalid Pixel = ',pixel
       call fatal_error(code//' Abort')
    endif

    call nest2ring_old(nside, pixel, pix_ring)
    call template_pixel_ring_old(nside, pix_ring, templ8, reflexion)

    return
  end subroutine template_pixel_nest_old

  !=======================================================================
  subroutine same_shape_pixels_nest_old(nside, templ8, list, reflexion, nrep) ! TO BE REMOVED
    !=======================================================================
    !   Returns the list of NESTED ordered pixels having the same shape as the
    !   template provided
    !-------------------------------------------------------------
    use num_rec, only : iindexx

    character(len=*), parameter :: code = "same_shape_pixels_nest_old"
    integer(I4B),    intent(IN)                        :: nside
    integer(I4B),    intent(IN)                        :: templ8
    integer(I4B),    pointer,   dimension(:), optional :: list
    integer(I4B),    pointer,   dimension(:), optional :: reflexion
    integer(I4B),    intent(OUT)            , optional :: nrep

    integer(i4b) :: ntplt
    integer(i4b) :: i
    integer(I4B),   allocatable,   dimension(:)   :: idx, tmp
    integer(i4b) :: ll, lr
    logical(lgt) :: do_list, do_rfx, do_nrep
    integer(i4b) :: nr
!     integer(I4B),    pointer,   dimension(:) :: list_in
!     integer(I4B),    pointer,   dimension(:) :: reflexion_in
!     integer(I4B),    intent(OUT)             :: nrep_in
    !-------------------------------------------------------------

    do_list = (present(list))
    do_rfx  = (present(reflexion))
    do_nrep = (present(nrep))
  
    if (do_rfx .and. .not. do_list) then
       print*,'Error in '//code//'. Can not have Reflexion without pixel list'
       call fatal_error
    endif

    ntplt= nside2ntemplates(nside)

    if (templ8 < 0 .or. templ8 >= ntplt) then
       print*,'Error on template argument'
       print*,'Nside = ',nside,', Template = ',templ8
       print*,'Template should be in [0, (1+Nside*(Nside+6))/4-1=',ntplt-1,']'
       call fatal_error(code//' Abort')
    endif

    ! makes pixel list in RING indexing
    call same_shape_pixels_ring_old( nside, templ8, list, reflexion, nr)
    if (do_nrep) nrep = nr

    if (do_list .or. do_rfx) then
       allocate(idx  (0:nr-1))
       allocate(tmp  (0:nr-1))

       ! convert to NESTED
       if (do_list) then
          ll = lbound(list,      dim=1)
          do i = 0, nr-1
             call ring2nest_old(nside, list(ll+i), list(ll+i))
          enddo

          ! sort arrays in increasing order of pixel NESTED index
          call iindexx(nr, list, idx)
          !!!!!  list(ll:nr-1+ll) = list(idx-1+ll) ! replaced by loops for Gfortran
          do i=0, nr-1
             tmp(i) = list(idx(i)-1+ll)
          enddo
          do i=0, nr-1
             list(ll+i) = tmp(i)
          enddo
       endif
  
       if (do_rfx) then
          lr = lbound(reflexion,      dim=1)
          !!!!! reflexion(lr:nr-1+lr) = reflexion(idx-1+lr) ! replaced by loops for Gfortran
          do i=0, nr-1
             tmp(i) = reflexion(idx(i)-1+lr)
          enddo
          do i=0, nr-1
             reflexion(lr+i) = tmp(i)
          enddo
       endif

       deallocate(tmp)
       deallocate(idx)
    endif

    return
  end subroutine same_shape_pixels_nest_old

end module pix_tools
