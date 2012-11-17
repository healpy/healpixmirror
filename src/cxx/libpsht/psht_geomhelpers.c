/*
 *  This file is part of libpsht.
 *
 *  libpsht is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  libpsht is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libpsht; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 *  libpsht is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*! \file psht_geomhelpers.c
 *  Spherical transform library
 *
 *  Copyright (C) 2006-2012 Max-Planck-Society
 *  Copyright (C) 2007-2008 Pavel Holoborodko (for gauss_legendre_tbl)
 *  \author Martin Reinecke \author Pavel Holoborodko
 */

#include <math.h>
#include "psht_geomhelpers.h"
#include "c_utils.h"

#if 0

#include "svd_c.h"

static void make_healpix_weights (int nside, double *weight)
  {
  int lmax = (int)(3.5*nside);
  double dth1 = 1./(3*nside*nside);
  double dth2 = 2./(3*nside);
  int nring = 2*nside;
  int npix = 12*nside*nside;
  double **mat, *z=RALLOC(double,nring);
  int *nir=RALLOC(int,nring);
  double *b=RALLOC(double,nring);
  svd_obj svd;
  int ith, l;

  ALLOC2D(mat,double,lmax/2+1,nring);
  for (ith=0; ith<nring; ++ith)
    {
    if (ith<nside-1)
      {
      nir[ith] = 8*(ith+1);
      z[ith] = 1 - dth1*(ith+1)*(ith+1);
      }
    else
      {
      nir[ith]=8*nside;
      z[ith] = (2*nside-ith-1)*dth2;
      }
    }
  nir[nring-1]/=2;

  for (l=0; l<=lmax/2; ++l)
    for (ith=0; ith<nring; ++ith)
      mat[l][ith]=0;

  for (ith=0; ith<nring; ++ith)
    {
    double p0 = 1;
    double p1 = z[ith];
    mat[0][ith] = p0;
    for (l=2; l<=lmax; ++l)
      {
      double p2 = z[ith]*p1*(2*l-1) - p0*(l-1);
      p2/=l;
      if ((l%2)==0) mat[l/2][ith] = p2;
      p0 = p1;
      p1 = p2;
      }
    }

  for (l=0; l<=lmax/2; ++l)
    {
    double bb=0;
    for (ith=0; ith<nring; ++ith)
      bb+=mat[l][ith]*nir[ith];
    b[l] = -bb;
    }
  b[0] += npix;

  svd_init(mat,1e-14,lmax/2+1,nring,&svd);
  svd_solve(&svd,b);
  svd_destroy(&svd);
  for (l=0;l<nring;++l)
    weight[l]=weight[2*nring-l-2] = 1.+b[l]/nir[l];

  DEALLOC2D(mat);
  DEALLOC(b);
  DEALLOC(z);
  DEALLOC(nir);
  }

#endif

void psht_make_healpix_geom_info (int nside, int stride,
  psht_geom_info **geom_info)
  {
  double *weight=RALLOC(double,2*nside);
  SET_ARRAY(weight,0,2*nside,1);
  psht_make_weighted_healpix_geom_info (nside, stride, weight, geom_info);
  DEALLOC(weight);
  }

void psht_make_weighted_healpix_geom_info (int nside, int stride,
  const double *weight, psht_geom_info **geom_info)
  {
  const double pi=3.141592653589793238462643383279502884197;
  ptrdiff_t npix=(ptrdiff_t)nside*nside*12;
  ptrdiff_t ncap=2*(ptrdiff_t)nside*(nside-1);
  int nrings=4*nside-1;

  double *theta=RALLOC(double,nrings);
  double *weight_=RALLOC(double,nrings);
  int *nph=RALLOC(int,nrings);
  double *phi0=RALLOC(double,nrings);
  ptrdiff_t *ofs=RALLOC(ptrdiff_t,nrings);
  int *stride_=RALLOC(int,nrings);
  int m;
  for (m=0; m<nrings; ++m)
    {
    int ring=m+1;
    ptrdiff_t northring = (ring>2*nside) ? 4*nside-ring : ring;
    stride_[m] = stride;
    if (northring < nside)
      {
      theta[m] = 2*asin(northring/(sqrt(6.)*nside));
      nph[m] = 4*northring;
      phi0[m] = pi/nph[m];
      ofs[m] = 2*northring*(northring-1)*stride;
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
      ofs[m] = (ncap + (northring-nside)*nph[m])*stride;
      }
    if (northring != ring) /* southern hemisphere */
      {
      theta[m] = pi-theta[m];
      ofs[m] = (npix - nph[m])*stride - ofs[m];
      }
    weight_[m]=4.*pi/npix*weight[northring-1];
    }

#if 0
  {
  double *w2=RALLOC(double,nrings);
  make_healpix_weights(nside,w2);
  for (m=0; m<nrings; ++m)
    weight_[m]*=w2[m];
  DEALLOC(w2);
  }
#endif

  psht_make_geom_info (nrings, nph, ofs, stride_, phi0, theta, weight_,
    geom_info);

  DEALLOC(theta);
  DEALLOC(weight_);
  DEALLOC(nph);
  DEALLOC(phi0);
  DEALLOC(ofs);
  DEALLOC(stride_);
  }

static void makeweights (int bw, double *weights)
  {
  const double pi = 3.141592653589793238462643383279502884197;
  const double fudge = pi/(4*bw);
  int j;
  for (j=0; j<2*bw; ++j)
    {
    double tmpsum = 0;
    int k;
    for (k=0; k<bw; ++k)
      tmpsum += 1./(2*k+1) * sin((2*j+1)*(2*k+1)*fudge);
    tmpsum *= sin((2*j+1)*fudge);
    tmpsum *= 2./bw;
    weights[j] = tmpsum;
    /* weights[j + 2*bw] = tmpsum * sin((2*j+1)*fudge); */
    }
  }

/* Function adapted from GNU GSL file glfixed.c
   Original author: Pavel Holoborodko (http://www.holoborodko.com)

   Adjustments by M. Reinecke
    - adjusted interface (keep epsilon internal, return full number of points)
    - removed precomputed tables
    - tweaked Newton iteration to obtain higher accuracy */
static void gauss_legendre_tbl(int n, double* x, double* w)
  {
  const double pi = 3.141592653589793238462643383279502884197;
  const double eps = 3e-14;
  int i, k, m = (n+1)>>1;

  double t0 = 1 - (1-1./n) / (8.*n*n);
  double t1 = 1./(4.*n+2.);

  for (i=1; i<=m; ++i)
    {
    double x0 = cos(pi * ((i<<2)-1) * t1) * t0;

    int dobreak=0;
    int j=0;
    double dpdx;
    while(1)
      {
      double P_1 = 1.0;
      double P0 = x0;
      double dx, x1;

      for (k=2; k<=n; k++)
        {
        double P_2 = P_1;
        P_1 = P0;
//        P0 = ((2*k-1)*x0*P_1-(k-1)*P_2)/k;
        P0 = x0*P_1 + (k-1.)/k * (x0*P_1-P_2);
        }

//      dpdx = ((x0*P0 - P_1) * n) / ((x0-1.)*(x0+1.));
      dpdx = (x0*P0 - P_1) * n / (x0*x0-1.);

      /* Newton step */
      x1 = x0 - P0/dpdx;
      dx = x0-x1;
      x0 = x1;
      if (dobreak) break;

      if (fabs(dx)<=eps) dobreak=1;
      UTIL_ASSERT(++j<100,"convergence problem");
      }

    x[i-1] = -x0;
    x[n-i] = x0;
//    w[i-1] = w[n-i] = 2. / (((1.-x0)*(1.+x0)) * dpdx * dpdx);
    w[i-1] = w[n-i] = 2. / ((1.-x0*x0) * dpdx * dpdx);
    }
  }

void psht_make_gauss_geom_info (int nrings, int nphi, int stride,
  psht_geom_info **geom_info)
  {
  psht_make_gauss_geom_info_2(nrings,nphi,stride,stride*nphi,geom_info);
  }

void psht_make_gauss_geom_info_2 (int nrings, int nphi, int stride_lon,
  int stride_lat, psht_geom_info **geom_info)
  {
  const double pi=3.141592653589793238462643383279502884197;

  double *theta=RALLOC(double,nrings);
  double *weight=RALLOC(double,nrings);
  int *nph=RALLOC(int,nrings);
  double *phi0=RALLOC(double,nrings);
  ptrdiff_t *ofs=RALLOC(ptrdiff_t,nrings);
  int *stride_=RALLOC(int,nrings);
  int m;

  gauss_legendre_tbl(nrings,theta,weight);

  for (m=0; m<nrings; ++m)
    {
    theta[m] = acos(-theta[m]);
    nph[m]=nphi;
    phi0[m]=0;
    ofs[m]=(ptrdiff_t)m*stride_lat;
    stride_[m]=stride_lon;
    weight[m]*=2*pi/nphi;
    }

  psht_make_geom_info (nrings, nph, ofs, stride_, phi0, theta, weight,
    geom_info);

  DEALLOC(theta);
  DEALLOC(weight);
  DEALLOC(nph);
  DEALLOC(phi0);
  DEALLOC(ofs);
  DEALLOC(stride_);
  }

void psht_make_ecp_geom_info (int nrings, int nphi, double phi0, int stride,
  psht_geom_info **geom_info)
  {
  psht_make_ecp_geom_info_2(nrings, nphi, phi0, stride, stride*nphi, geom_info);
  }

void psht_make_ecp_geom_info_2 (int nrings, int nphi, double phi0,
  int stride_lon, int stride_lat, psht_geom_info **geom_info)
  {
  const double pi=3.141592653589793238462643383279502884197;

  double *theta=RALLOC(double,nrings);
  double *weight=RALLOC(double,nrings);
  int *nph=RALLOC(int,nrings);
  double *phi0_=RALLOC(double,nrings);
  ptrdiff_t *ofs=RALLOC(ptrdiff_t,nrings);
  int *stride_=RALLOC(int,nrings);

  int m;

  UTIL_ASSERT((nrings&1)==0,
    "Even number of rings needed for equidistant grid!");
  makeweights(nrings/2,weight);
  for (m=0; m<nrings; ++m)
    {
    theta[m] = (m+0.5)*pi/nrings;
    nph[m]=nphi;
    phi0_[m]=phi0;
    ofs[m]=(ptrdiff_t)m*stride_lat;
    stride_[m]=stride_lon;
    weight[m]*=2*pi/nphi;
    }

  psht_make_geom_info (nrings, nph, ofs, stride_, phi0_, theta, weight,
    geom_info);

  DEALLOC(theta);
  DEALLOC(weight);
  DEALLOC(nph);
  DEALLOC(phi0_);
  DEALLOC(ofs);
  DEALLOC(stride_);
  }
