#include "c_utils.h"
#include "psht.h"
#include "psht_geomhelpers.h"
#include "psht_almhelpers.h"
#include "ls_cfortran.h"

static void psht_make_healpix_geom_info_2 (int nside, double *wgt,
  double z1, double z2, psht_geom_info **geom_info)
  {
  const double pi=3.141592653589793238462643383279502884197;
  int npix=nside*nside*12;
  int ncap=2*nside*(nside-1);
  int nrings=4*nside-1;

  double *theta=RALLOC(double,nrings);
  double *weight=RALLOC(double,nrings);
  int *nph=RALLOC(int,nrings);
  double *phi0=RALLOC(double,nrings);
  int *ofs=RALLOC(int,nrings);
  int *stride=RALLOC(int,nrings);
  int m=0,i;
  for (i=0; i<nrings; ++i)
    {
    int ring=i+1;
    int northring = (ring>2*nside) ? 4*nside-ring : ring;
    if (northring < nside)
      {
      theta[m] = 2*asin(northring/(sqrt(6.)*nside));
      nph[m] = 4*northring;
      phi0[m] = pi/nph[m];
      ofs[m] = 2*northring*(northring-1);
      }
    else
      {
      double fact1 = (8.*nside)/npix;
      double costheta = (2*nside-northring)*fact1;
      theta[m] = acos(costheta);
      nph[m] = 4*nside;
      if ((northring-nside) & 1)
        phi0[m] = 0;
      else
        phi0[m] = pi/nph[m];
      ofs[m] = ncap + (northring-nside)*nph[m];
      }
    if (northring != ring) /* southern hemisphere */
      {
      theta[m] = pi-theta[m];
      ofs[m] = npix - nph[m] - ofs[m];
      }
    weight[m]=4.*pi/npix*wgt[northring-1];
    stride[m]=1;

/* decide whether the ring must be used */
    {
    double ctheta = cos(theta[m]);
    if (z1<z2)
      {
      if ((ctheta>z1) && (ctheta<z2))
        ++m;
      }
    else
      {
      if ((ctheta>z1) || (ctheta<z2))
        ++m;
      }
    }
    }

  psht_make_geom_info (m, nph, ofs, stride, phi0, theta, weight, geom_info);

  DEALLOC(theta);
  DEALLOC(weight);
  DEALLOC(nph);
  DEALLOC(phi0);
  DEALLOC(ofs);
  DEALLOC(stride);
  }

#define CONCAT(a,b) a ## b

#define FLT double
#define X(arg) CONCAT(pshtd_,arg)
#define Y(arg) CONCAT(arg,_D)
#define Z(arg) CONCAT(arg,_d)
#define PFLT PDOUBLE
#include "psht_healpix_f_inc.c"
#undef PFLT
#undef FLT
#undef Z
#undef Y
#undef X

#define FLT float
#define X(arg) CONCAT(pshts_,arg)
#define Y(arg) CONCAT(arg,_S)
#define Z(arg) CONCAT(arg,_s)
#define PFLT PFLOAT
#include "psht_healpix_f_inc.c"
#undef PFLT
#undef FLT
#undef Z
#undef Y
#undef X

#undef CONCAT
