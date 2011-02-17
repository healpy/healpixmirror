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

/*! \file psht_test_mpi.c
    Accuracy test for libpsht's map analysis with MPI support.

    This program first generates a_lm coefficients up to
    a user-specified lmax (with mmax=lmax); where applicable, the
    real and imaginary parts of the coefficients are uniform
    random numbers of the interval [-1;1[.
    Afterwards, the random a_lm are converted to a map.
    This map is analyzed (optionally using an iterative scheme
    with a user-supplied number of steps).
    After every iteration, the code then outputs the RMS of the residual a_lm
    (i.e. the difference between the current and original a_lm), divided by
    the RMS of the original a_lm, as well as the maximum absolute change of any
    real or imaginary part between the current and original a_lm.

    This operation can be performed for several different pixelisations:
      - a Gaussian with the minimal number of rings for exact analysis
        and a user-defined ring resolution
      - an ECP grid with the minimal number of rings for exact analysis
        and a user-defined ring resolution
      - a Healpix grid with a user-defined Nside parameter.

    The user can specify the spin of the desired transform.

    Copyright (C) 2006-2011 Max-Planck-Society
    \author Martin Reinecke
*/

#ifdef USE_MPI

#include <stdio.h>
#include <string.h>
#include "psht_mpi.h"
#include "psht_geomhelpers.h"
#include "psht_almhelpers.h"
#include "c_utils.h"
#include "walltime_c.h"

int ntasks, mytask;

static double drand (double min, double max)
  {
  return min + (max-min)*rand()/(RAND_MAX+1.0);
  }

static ptrdiff_t get_nalms(const psht_alm_info *ainfo)
  {
  int i;
  ptrdiff_t res=0;
  for (i=0; i<ainfo->nm; ++i)
    res += ainfo->lmax-ainfo->mval[i]+1;
  return res;
  }

static ptrdiff_t get_npix(const psht_geom_info *ginfo)
  {
  int i;
  ptrdiff_t res=0;
  for (i=0; i<ginfo->npairs; ++i)
    {
    res += ginfo->pair[i].r1.nph;
    if (ginfo->pair[i].r2.nph>0) res += ginfo->pair[i].r2.nph;
    }
  return res;
  }

static void reduce_alm_info(psht_alm_info *ainfo)
  {
  int nmnew=0;
  int i;
  ptrdiff_t ofs = 0;
  for (i=mytask; i<ainfo->nm; i+=ntasks,++nmnew)
    {
    ainfo->mval[nmnew]=ainfo->mval[i];
    ainfo->mvstart[nmnew]=ofs-ainfo->mval[nmnew];
    ofs+=ainfo->lmax-ainfo->mval[nmnew]+1;
    }
  ainfo->nm=nmnew;
  }

static void reduce_geom_info(psht_geom_info *ginfo)
  {
  int npairsnew=0;
  int i;
  ptrdiff_t ofs = 0;
  for (i=mytask; i<ginfo->npairs; i+=ntasks,++npairsnew)
    {
    ginfo->pair[npairsnew]=ginfo->pair[i];
    ginfo->pair[npairsnew].r1.ofs=ofs;
    ofs+=ginfo->pair[npairsnew].r1.nph;
    ginfo->pair[npairsnew].r2.ofs=ofs;
    if (ginfo->pair[npairsnew].r2.nph>0) ofs+=ginfo->pair[npairsnew].r2.nph;
    }
  ginfo->npairs=npairsnew;
  }

static void random_alm (pshtd_cmplx *alm, psht_alm_info *helper, int spin)
  {
  static int cnt=0;
  int l,mi;
  ++cnt;
  for (mi=0;mi<helper->nm; ++mi)
    {
    int m=helper->mval[mi];
    srand(1234567*cnt+8912*m);
    for (l=m;l<=helper->lmax; ++l)
      {
      if ((l<spin)&&(m<spin))
        alm[psht_alm_index(helper,l,mi)] = pshtd_cmplx_null;
      else
        {
        alm[psht_alm_index(helper,l,mi)].re = drand(-1,1);
        alm[psht_alm_index(helper,l,mi)].im = (m==0) ? 0 : drand(-1,1);
        }
      }
    }
  }

static void measure_errors (pshtd_cmplx **alm, pshtd_cmplx **alm2,
  const psht_alm_info *ainfo, int ncomp)
  {
  int i,l,mi;
  long nalms=get_nalms(ainfo), nalms_tot;
  MPI_Allreduce(&nalms,&nalms_tot,1,MPI_LONG,MPI_SUM,MPI_COMM_WORLD);

  for (i=0; i<ncomp; ++i)
    {
    double sum=0, sum2=0, maxdiff=0, sumtot, sum2tot, maxdifftot;
    for (mi=0; mi<ainfo->nm; ++mi)
      {
      int m=ainfo->mval[mi];
      for (l=m; l<=ainfo->lmax; ++l)
        {
        ptrdiff_t idx=psht_alm_index(ainfo,l,mi);
        double x=alm[i][idx].re-alm2[i][idx].re,
               y=alm[i][idx].im-alm2[i][idx].im;
        sum+=x*x+y*y;
        sum2+=alm[i][idx].re*alm[i][idx].re+alm[i][idx].im*alm[i][idx].im;
        if (fabs(x)>maxdiff) maxdiff=fabs(x);
        if (fabs(y)>maxdiff) maxdiff=fabs(y);
        }
      }

    MPI_Allreduce(&sum,&sumtot,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD);
    MPI_Allreduce(&sum2,&sum2tot,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD);
    MPI_Allreduce(&maxdiff,&maxdifftot,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD);
    sumtot=sqrt(sumtot/nalms_tot);
    sum2tot=sqrt(sum2tot/nalms_tot);
    if (mytask==0)
      printf("component %i: rms %e, maxerr %e\n",i, sumtot/sum2tot, maxdifftot);
    }
  }

static void map2alm_iter (psht_geom_info *tinfo, double **map,
  pshtd_cmplx **alm_orig, pshtd_cmplx **alm, int lmax, int mmax,
  ptrdiff_t npix, int spin, int niter)
  {
  psht_alm_info *alms;
  pshtd_joblist *joblist;
  int ncomp = (spin==0) ? 1 : 2;
  int iter,i;
  ptrdiff_t m;
  double timer;

  psht_make_triangular_alm_info(lmax,mmax,1,&alms);
  reduce_alm_info(alms);
  pshtd_make_joblist (&joblist);

  if (spin==0)
    pshtd_add_job_map2alm(joblist,map[0],alm[0],0);
  else
    pshtd_add_job_map2alm_spin(joblist,map[0],map[1],alm[0],alm[1],spin,0);
MPI_Barrier(MPI_COMM_WORLD);
  timer=wallTime();
  pshtd_execute_jobs_mpi (joblist, tinfo, alms, MPI_COMM_WORLD);
MPI_Barrier(MPI_COMM_WORLD);
  if (mytask==0) printf("wall time for map2alm: %fs\n",wallTime()-timer);
  pshtd_clear_joblist (joblist);
  measure_errors(alm_orig,alm,alms,ncomp);

  for (iter=0; iter<niter; ++iter)
    {
    double **map2;
    ALLOC2D(map2,double,ncomp,npix);
    if (mytask==0) printf ("\niteration %i:\n", iter+1);
    if (spin==0)
      pshtd_add_job_alm2map(joblist,alm[0],map2[0],0);
    else
      pshtd_add_job_alm2map_spin(joblist,alm[0],alm[1],map2[0],map2[1],spin,0);
MPI_Barrier(MPI_COMM_WORLD);
    timer=wallTime();
    pshtd_execute_jobs_mpi (joblist, tinfo, alms, MPI_COMM_WORLD);
MPI_Barrier(MPI_COMM_WORLD);
    if (mytask==0) printf("wall time for alm2map: %fs\n",wallTime()-timer);
    pshtd_clear_joblist (joblist);
    for (i=0; i<ncomp; ++i)
      for (m=0; m<npix; ++m)
        map2[i][m] = map[i][m]-map2[i][m];

    if (spin==0)
      pshtd_add_job_map2alm(joblist,map2[0],alm[0],1);
    else
      pshtd_add_job_map2alm_spin(joblist,map2[0],map2[1],alm[0],alm[1],spin,1);
MPI_Barrier(MPI_COMM_WORLD);
    timer=wallTime();
    pshtd_execute_jobs_mpi (joblist, tinfo, alms, MPI_COMM_WORLD);
MPI_Barrier(MPI_COMM_WORLD);
    if (mytask==0) printf("wall time for map2alm: %fs\n",wallTime()-timer);
    pshtd_clear_joblist (joblist);
    DEALLOC2D(map2);
    measure_errors(alm_orig,alm,alms,ncomp);
    }

  psht_destroy_alm_info(alms);
  pshtd_destroy_joblist(joblist);
  }

static void check_accuracy (psht_geom_info *tinfo, ptrdiff_t lmax,
  ptrdiff_t mmax, ptrdiff_t npix, int spin, int niter)
  {
  psht_alm_info *alms;
  pshtd_joblist *joblist;
  double **map;
  pshtd_cmplx **alm, **alm2;
  ptrdiff_t nalms;
  int ncomp = (spin==0) ? 1 : 2;
  double timer;

  ALLOC2D(map,double,ncomp,npix);

  psht_make_triangular_alm_info(lmax,mmax,1,&alms);
  reduce_alm_info(alms);
  pshtd_make_joblist (&joblist);
  nalms=get_nalms(alms);

  srand(4);
  ALLOC2D(alm,pshtd_cmplx,ncomp,nalms);
  random_alm(alm[0],alms,spin);
  if (spin>0)
    random_alm(alm[1],alms,spin);

  ALLOC2D(alm2,pshtd_cmplx,ncomp,nalms);

  if (mytask==0) printf ("\niteration 0:\n");
  if (spin==0)
    pshtd_add_job_alm2map(joblist,alm[0],map[0],0);
  else
    pshtd_add_job_alm2map_spin(joblist,alm[0],alm[1],map[0],map[1],spin,0);
MPI_Barrier(MPI_COMM_WORLD);
  timer=wallTime();
  pshtd_execute_jobs_mpi (joblist, tinfo, alms, MPI_COMM_WORLD);
MPI_Barrier(MPI_COMM_WORLD);
  if (mytask==0) printf("wall time for alm2map: %fs\n",wallTime()-timer);
  pshtd_clear_joblist (joblist);

  map2alm_iter(tinfo, map, alm, alm2, lmax, mmax, npix, spin, niter);

  DEALLOC2D(map);
  DEALLOC2D(alm);
  DEALLOC2D(alm2);

  psht_destroy_alm_info(alms);
  pshtd_destroy_joblist(joblist);
  }

int main(int argc, char **argv)
  {
  int lmax;
  int spin;
  int niter;
  psht_geom_info *tinfo;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&ntasks);
  MPI_Comm_rank(MPI_COMM_WORLD,&mytask);

  module_startup_c("psht_test_mpi",argc,6,
    "<healpix|ecp|gauss> <lmax> <nside|nphi> <niter> <spin>",mytask==0);
  lmax=atoi(argv[2]);
  niter=atoi(argv[4]);
  spin=atoi(argv[5]);

  if (mytask==0)
    {
    printf("Testing map analysis accuracy.\n");
    printf("lmax=%d, %d iterations, spin=%d\n", lmax, niter, spin);
    }

  if (strcmp(argv[1],"gauss")==0)
    {
    int nrings=lmax+1;
    int ppring=atoi(argv[3]);
    ptrdiff_t npix=(ptrdiff_t)nrings*ppring;
    if (mytask==0)
      printf("\nTesting Gaussian grid (%d rings, %d pixels/ring, %ld pixels)\n",
             nrings,ppring,(long)npix);
    psht_make_gauss_geom_info (nrings, ppring, 1, &tinfo);
    reduce_geom_info(tinfo);
    npix=get_npix(tinfo);
    check_accuracy(tinfo,lmax,lmax,npix,spin,niter);
    psht_destroy_geom_info(tinfo);
    }
  else if (strcmp(argv[1],"ecp")==0)
    {
    int nrings=2*lmax+2;
    int ppring=atoi(argv[3]);
    ptrdiff_t npix=(ptrdiff_t)nrings*ppring;
    if (mytask==0)
      printf("\nTesting ECP grid (%d rings, %d pixels/ring, %ld pixels)\n",
             nrings,ppring,(long)npix);
    psht_make_ecp_geom_info (nrings, ppring, 0., 1, &tinfo);
    reduce_geom_info(tinfo);
    npix=get_npix(tinfo);
    check_accuracy(tinfo,lmax,lmax,npix,spin,niter);
    psht_destroy_geom_info(tinfo);
    }
  else if (strcmp(argv[1],"healpix")==0)
    {
    int nside=atoi(argv[3]);
    ptrdiff_t npix;
    if (nside<1) nside=1;
    npix=12*(ptrdiff_t)nside*nside;
    if (mytask==0)
      printf("\nTesting Healpix grid (nside=%d, %ld pixels)\n",
             nside,(long)npix);
    psht_make_healpix_geom_info (nside, 1, &tinfo);
    reduce_geom_info(tinfo);
    npix=get_npix(tinfo);
    check_accuracy(tinfo,lmax,lmax,npix,spin,niter);
    psht_destroy_geom_info(tinfo);
    }
  else
    UTIL_FAIL("unknown grid geometry");

  MPI_Finalize();
  return 0;
  }

#else

#include "c_utils.h"

int main(void)
  { UTIL_FAIL("MPI support not enabled."); return 1; }

#endif
