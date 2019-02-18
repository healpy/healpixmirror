module healpix_sharp_f90
use, intrinsic :: iso_c_binding
implicit none
public

interface

subroutine sharp_hp_alm2map_x_s(nside, lmax, mmax, alm, map, zbounds) bind(C, name="sharps_alm2map")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  complex(c_float), intent(in) :: alm(*)
  real(c_float), intent(out) :: map(*)
  real(c_double), intent(in) :: zbounds(*)
end subroutine

subroutine sharp_hp_alm2map_x_d(nside, lmax, mmax, alm, map, zbounds) bind(C, name="sharpd_alm2map")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  complex(c_double), intent(in) :: alm(*)
  real(c_double), intent(out) :: map(*)
  real(c_double), intent(in) :: zbounds(*)
end subroutine

subroutine sharp_hp_map2alm_x_s(nside, lmax, mmax, map, alm, zbounds, wgt) bind(C, name="sharps_map2alm")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  real(c_float), intent(in) :: map(*)
  complex(c_float), intent(out) :: alm(*)
  real(c_double), intent(in) :: zbounds(*)
  real(c_double), intent(in) :: wgt(*)
end subroutine

subroutine sharp_hp_map2alm_x_d(nside, lmax, mmax, map, alm, zbounds, wgt) bind(C, name="sharpd_map2alm")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  real(c_double), intent(in) :: map(*)
  complex(c_double), intent(out) :: alm(*)
  real(c_double), intent(in) :: zbounds(*)
  real(c_double), intent(in) :: wgt(*)
end subroutine

subroutine sharp_hp_alm2map_pol_x_s(nside, lmax, mmax, alm, map, zbounds) bind(C, name="sharps_alm2map_pol")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  complex(c_float), intent(in) :: alm(*)
  real(c_float), intent(out) :: map(*)
  real(c_double), intent(in) :: zbounds(*)
end subroutine

subroutine sharp_hp_alm2map_pol_x_d(nside, lmax, mmax, alm, map, zbounds) bind(C, name="sharpd_alm2map_pol")
  use iso_c_binding
  integer(c_int), intent(in), value :: nside, lmax, mmax
  complex(c_double), intent(in) :: alm(*)
  real(c_double), intent(out) :: map(*)
  real(c_double), intent(in) :: zbounds(*)
end subroutine

!static void X(hp_alm2map_pol) (int nside, int lmax, int mmax, FLT *alm,
!			       FLT *map, double *zbounds)

end interface

end module
