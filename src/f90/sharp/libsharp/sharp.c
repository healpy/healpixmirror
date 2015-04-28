/*
 *  This file is part of libsharp.
 *
 *  libsharp is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  libsharp is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libsharp; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/*
 *  libsharp is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*! \file sharp.c
 *  Spherical transform library
 *
 *  Copyright (C) 2006-2012 Max-Planck-Society
 *  \author Martin Reinecke \author Dag Sverre Seljebotn
 */

#include <math.h>
#include "ls_fft.h"
#include "sharp_ylmgen_c.h"
#include "sharp_internal.h"
#include "c_utils.h"
#include "sharp_core.h"
#include "sharp_vecutil.h"
#include "walltime_c.h"
#include "sharp_almhelpers.h"
#include "sharp_geomhelpers.h"

typedef complex double dcmplx;
typedef complex float  fcmplx;

static int chunksize_min=500, nchunks_max=10;

static void get_chunk_info (int ndata, int nmult, int *nchunks, int *chunksize)
  {
  *chunksize = IMAX(chunksize_min,(ndata+nchunks_max-1)/nchunks_max);
  *chunksize = ((*chunksize+nmult-1)/nmult)*nmult;
  *nchunks = (ndata+*chunksize-1) / *chunksize;
  }

typedef struct
  {
  double s;
  int i;
  } idxhelper;

static int idx_compare (const void *xa, const void *xb)
  {
  const idxhelper *a=xa, *b=xb;
  return (a->s > b->s) ? -1 : (a->s < b->s) ? 1 : 0;
  }

typedef struct
  {
  double phi0_;
  dcmplx *shiftarr, *work;
  int s_shift, s_work;
  real_plan plan;
  int norot;
  } ringhelper;

static void ringhelper_init (ringhelper *self)
  {
  static ringhelper rh_null = { 0, NULL, NULL, 0, 0, NULL, 0 };
  *self = rh_null;
  }

static void ringhelper_destroy (ringhelper *self)
  {
  if (self->plan) kill_real_plan(self->plan);
  DEALLOC(self->shiftarr);
  DEALLOC(self->work);
  ringhelper_init(self);
  }

static void ringhelper_update (ringhelper *self, int nph, int mmax, double phi0)
  {
  self->norot = (fabs(phi0)<1e-14);
  if (!(self->norot))
    if ((mmax!=self->s_shift-1) || (!FAPPROX(phi0,self->phi0_,1e-12)))
      {
      RESIZE (self->shiftarr,dcmplx,mmax+1);
      self->s_shift = mmax+1;
      self->phi0_ = phi0;
      for (int m=0; m<=mmax; ++m)
        self->shiftarr[m] = cos(m*phi0) + _Complex_I*sin(m*phi0);
      }
  if (!self->plan) self->plan=make_real_plan(nph);
  if (nph!=(int)self->plan->length)
    {
    kill_real_plan(self->plan);
    self->plan=make_real_plan(nph);
    }
  GROW(self->work,dcmplx,self->s_work,nph);
  }

static int ringinfo_compare (const void *xa, const void *xb)
  {
  const hpsharp_ringinfo *a=xa, *b=xb;
  return (a->sth < b->sth) ? -1 : (a->sth > b->sth) ? 1 : 0;
  }
static int ringpair_compare (const void *xa, const void *xb)
  {
  const hpsharp_ringpair *a=xa, *b=xb;
  if (a->r1.nph==b->r1.nph)
    return (a->r1.phi0 < b->r1.phi0) ? -1 :
      ((a->r1.phi0 > b->r1.phi0) ? 1 :
        (a->r1.cth>b->r1.cth ? -1 : 1));
  return (a->r1.nph<b->r1.nph) ? -1 : 1;
  }

void hpsharp_make_general_alm_info (int lmax, int nm, int stride, const int *mval,
  const ptrdiff_t *mstart, hpsharp_alm_info **alm_info)
  {
  hpsharp_alm_info *info = RALLOC(hpsharp_alm_info,1);
  info->lmax = lmax;
  info->nm = nm;
  info->mval = RALLOC(int,nm);
  info->mvstart = RALLOC(ptrdiff_t,nm);
  info->stride = stride;
  for (int mi=0; mi<nm; ++mi)
    {
    info->mval[mi] = mval[mi];
    info->mvstart[mi] = mstart[mi];
    }
  *alm_info = info;
  }

void hpsharp_make_alm_info (int lmax, int mmax, int stride,
  const ptrdiff_t *mstart, hpsharp_alm_info **alm_info)
  {
  int *mval=RALLOC(int,mmax+1);
  for (int i=0; i<=mmax; ++i)
    mval[i]=i;
  hpsharp_make_general_alm_info (lmax, mmax+1, stride, mval, mstart, alm_info);
  DEALLOC(mval);
  }

ptrdiff_t hpsharp_alm_index (const hpsharp_alm_info *self, int l, int mi)
  { return self->mvstart[mi]+self->stride*l; }

void hpsharp_destroy_alm_info (hpsharp_alm_info *info)
  {
  DEALLOC (info->mval);
  DEALLOC (info->mvstart);
  DEALLOC (info);
  }

void hpsharp_make_geom_info (int nrings, const int *nph, const ptrdiff_t *ofs,
  const int *stride, const double *phi0, const double *theta,
  const double *wgt, hpsharp_geom_info **geom_info)
  {
  hpsharp_geom_info *info = RALLOC(hpsharp_geom_info,1);
  hpsharp_ringinfo *infos = RALLOC(hpsharp_ringinfo,nrings);

  int pos=0;
  info->pair=RALLOC(hpsharp_ringpair,nrings);
  info->npairs=0;
  *geom_info = info;

  for (int m=0; m<nrings; ++m)
    {
    infos[m].theta = theta[m];
    infos[m].cth = cos(theta[m]);
    infos[m].sth = sin(theta[m]);
    infos[m].weight = (wgt != NULL) ? wgt[m] : 1.;
    infos[m].phi0 = phi0[m];
    infos[m].ofs = ofs[m];
    infos[m].stride = stride[m];
    infos[m].nph = nph[m];
    }
  qsort(infos,nrings,sizeof(hpsharp_ringinfo),ringinfo_compare);
  while (pos<nrings)
    {
    info->pair[info->npairs].r1=infos[pos];
    if ((pos<nrings-1) && FAPPROX(infos[pos].cth,-infos[pos+1].cth,1e-12))
      {
      if (infos[pos].cth>0)  // make sure northern ring is in r1
        info->pair[info->npairs].r2=infos[pos+1];
      else
        {
        info->pair[info->npairs].r1=infos[pos+1];
        info->pair[info->npairs].r2=infos[pos];
        }
      ++pos;
      }
    else
      info->pair[info->npairs].r2.nph=-1;
    ++pos;
    ++info->npairs;
    }
  DEALLOC(infos);

  qsort(info->pair,info->npairs,sizeof(hpsharp_ringpair),ringpair_compare);
  }

void hpsharp_destroy_geom_info (hpsharp_geom_info *geom_info)
  {
  DEALLOC (geom_info->pair);
  DEALLOC (geom_info);
  }

/* This currently requires all m values from 0 to nm-1 to be present.
   It might be worthwhile to relax this criterion such that holes in the m
   distribution are permissible. */
static int hpsharp_get_mmax (int *mval, int nm)
  {
  int *mcheck=RALLOC(int,nm);
  SET_ARRAY(mcheck,0,nm,0);
  for (int i=0; i<nm; ++i)
    {
    int m_cur=mval[i];
    UTIL_ASSERT((m_cur>=0) && (m_cur<nm), "not all m values are present");
    UTIL_ASSERT(mcheck[m_cur]==0, "duplicate m value");
    mcheck[m_cur]=1;
    }
  DEALLOC(mcheck);
  return nm-1;
  }

static void ringhelper_phase2ring (ringhelper *self,
  const hpsharp_ringinfo *info, void *data, int mmax, const dcmplx *phase,
  int pstride, int flags)
  {
  int nph = info->nph;
  int stride = info->stride;

  ringhelper_update (self, nph, mmax, info->phi0);
  self->work[0]=phase[0];
  SET_ARRAY(self->work,1,nph,0.);

#if 0
  if (self->norot)
    for (int m=1; m<=mmax; ++m)
      {
      int idx1 = m%nph;
      int idx2 = nph-1-((m-1)%nph);
      self->work[idx1]+=phase[m*pstride];
      self->work[idx2]+=conj(phase[m*pstride]);
      }
  else
    for (int m=1; m<=mmax; ++m)
      {
      int idx1 = m%nph;
      int idx2 = nph-1-((m-1)%nph);
      dcmplx tmp = phase[m*pstride]*self->shiftarr[m];
      self->work[idx1]+=tmp;
      self->work[idx2]+=conj(tmp);
      }
#else
  int idx1=1, idx2=nph-1;
  for (int m=1; m<=mmax; ++m)
    {
    dcmplx tmp = phase[m*pstride];
    if(!self->norot) tmp*=self->shiftarr[m];
    self->work[idx1]+=tmp;
    self->work[idx2]+=conj(tmp);
    if (++idx1>=nph) idx1=0;
    if (--idx2<0) idx2=nph-1;
    }
#endif
  real_plan_backward_c (self->plan, (double *)(self->work));
  double wgt = (flags&SHARP_USE_WEIGHTS) ? info->weight : 1.;
  if (flags&SHARP_DP)
    for (int m=0; m<nph; ++m)
      ((double *)data)[m*stride+info->ofs]+=creal(self->work[m])*wgt;
  else
    for (int m=0; m<nph; ++m)
      ((float *)data)[m*stride+info->ofs] += (float)(creal(self->work[m])*wgt);
  }

static void ringhelper_ring2phase (ringhelper *self,
  const hpsharp_ringinfo *info, const void *data, int mmax, dcmplx *phase,
  int pstride, int flags)
  {
  int nph = info->nph;
#if 1
  int maxidx = mmax; /* Enable this for traditional Healpix compatibility */
#else
  int maxidx = IMIN(nph-1,mmax);
#endif

  ringhelper_update (self, nph, mmax, -info->phi0);
  double wgt = (flags&SHARP_USE_WEIGHTS) ? info->weight : 1;
  if (flags&SHARP_DP)
    for (int m=0; m<nph; ++m)
      self->work[m] = ((double *)data)[info->ofs+m*info->stride]*wgt;
  else
    for (int m=0; m<nph; ++m)
      self->work[m] = ((float *)data)[info->ofs+m*info->stride]*wgt;

  real_plan_forward_c (self->plan, (double *)self->work);

  if (self->norot)
    for (int m=0; m<=maxidx; ++m)
      phase[m*pstride] = self->work[m%nph];
  else
    for (int m=0; m<=maxidx; ++m)
      phase[m*pstride]=self->work[m%nph]*self->shiftarr[m];

  for (int m=maxidx+1;m<=mmax; ++m)
    phase[m*pstride]=0.;
  }

static void ringhelper_pair2phase (ringhelper *self, int mmax,
  const hpsharp_ringpair *pair, const void *data, dcmplx *phase1, dcmplx *phase2,
  int pstride, int flags)
  {
  ringhelper_ring2phase (self,&(pair->r1),data,mmax,phase1,pstride,flags);
  if (pair->r2.nph>0)
    ringhelper_ring2phase (self,&(pair->r2),data,mmax,phase2,pstride,flags);
  }

static void ringhelper_phase2pair (ringhelper *self, int mmax,
  const dcmplx *phase1, const dcmplx *phase2, int pstride,
  const hpsharp_ringpair *pair, void *data, int flags)
  {
  ringhelper_phase2ring (self,&(pair->r1),data,mmax,phase1,pstride,flags);
  if (pair->r2.nph>0)
    ringhelper_phase2ring (self,&(pair->r2),data,mmax,phase2,pstride,flags);
  }

static void fill_map (const hpsharp_geom_info *ginfo, void *map, double value,
  int flags)
  {
  for (int j=0;j<ginfo->npairs;++j)
    {
    if (flags&SHARP_DP)
      {
      for (int i=0;i<ginfo->pair[j].r1.nph;++i)
        ((double *)map)[ginfo->pair[j].r1.ofs+i*ginfo->pair[j].r1.stride]=value;
      for (int i=0;i<ginfo->pair[j].r2.nph;++i)
        ((double *)map)[ginfo->pair[j].r2.ofs+i*ginfo->pair[j].r2.stride]=value;
      }
    else
      {
      for (int i=0;i<ginfo->pair[j].r1.nph;++i)
        ((float *)map)[ginfo->pair[j].r1.ofs+i*ginfo->pair[j].r1.stride]
          =(float)value;
      for (int i=0;i<ginfo->pair[j].r2.nph;++i)
        ((float *)map)[ginfo->pair[j].r2.ofs+i*ginfo->pair[j].r2.stride]
          =(float)value;
      }
    }
  }

static void fill_alm (const hpsharp_alm_info *ainfo, void *alm, dcmplx value,
  int flags)
  {
  if (flags&SHARP_DP)
    for (int mi=0;mi<ainfo->nm;++mi)
      for (int l=ainfo->mval[mi];l<=ainfo->lmax;++l)
        ((dcmplx *)alm)[hpsharp_alm_index(ainfo,l,mi)] = value;
  else
    for (int mi=0;mi<ainfo->nm;++mi)
      for (int l=ainfo->mval[mi];l<=ainfo->lmax;++l)
        ((fcmplx *)alm)[hpsharp_alm_index(ainfo,l,mi)] = (fcmplx)value;
  }

static void init_output (hpsharp_job *job)
  {
  if (job->flags&SHARP_ADD) return;
  if (job->type == SHARP_MAP2ALM)
    for (int i=0; i<job->ntrans*job->nalm; ++i)
      fill_alm (job->ainfo,job->alm[i],0.,job->flags);
  else
    for (int i=0; i<job->ntrans*job->nmaps; ++i)
      fill_map (job->ginfo,job->map[i],0.,job->flags);
  }

static void alloc_phase (hpsharp_job *job, int nm, int ntheta)
  { job->phase=RALLOC(dcmplx,2*job->ntrans*job->nmaps*nm*ntheta); }

static void dealloc_phase (hpsharp_job *job)
  { DEALLOC(job->phase); }

//FIXME: set phase to zero if not SHARP_MAP2ALM?
static void map2phase (hpsharp_job *job, int mmax, int llim, int ulim)
  {
  if (job->type != SHARP_MAP2ALM) return;
  int pstride = 2*job->ntrans*job->nmaps;
#pragma omp parallel
{
  ringhelper helper;
  ringhelper_init(&helper);
#pragma omp for schedule(dynamic,1)
  for (int ith=llim; ith<ulim; ++ith)
    {
    int dim2 = pstride*(ith-llim)*(mmax+1);
    for (int i=0; i<job->ntrans*job->nmaps; ++i)
      ringhelper_pair2phase(&helper,mmax,&job->ginfo->pair[ith], job->map[i],
        &job->phase[dim2+2*i], &job->phase[dim2+2*i+1], pstride, job->flags);
    }
  ringhelper_destroy(&helper);
} /* end of parallel region */
  }

static void alloc_almtmp (hpsharp_job *job, int lmax)
  { job->almtmp=RALLOC(dcmplx,job->ntrans*job->nalm*(lmax+1)); }

static void dealloc_almtmp (hpsharp_job *job)
  { DEALLOC(job->almtmp); }

static void alm2almtmp (hpsharp_job *job, int lmax, int mi)
  {
  if (job->type!=SHARP_MAP2ALM)
    {
    ptrdiff_t ofs=job->ainfo->mvstart[mi];
    int stride=job->ainfo->stride;
    if (job->spin==0)
      {
      if (job->flags&SHARP_DP)
        for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
          for (int i=0; i<job->ntrans*job->nalm; ++i)
            job->almtmp[job->ntrans*job->nalm*l+i]
              = ((dcmplx *)job->alm[i])[ofs+l*stride];
      else
        for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
          for (int i=0; i<job->ntrans*job->nalm; ++i)
            job->almtmp[job->ntrans*job->nalm*l+i]
              = ((fcmplx *)job->alm[i])[ofs+l*stride];
      }
    else
      {
      if (job->flags&SHARP_DP)
        for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
          for (int i=0; i<job->ntrans*job->nalm; ++i)
            job->almtmp[job->ntrans*job->nalm*l+i]
              = ((dcmplx *)job->alm[i])[ofs+l*stride]*job->norm_l[l];
      else
        for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
          for (int i=0; i<job->ntrans*job->nalm; ++i)
            job->almtmp[job->ntrans*job->nalm*l+i]
              = ((fcmplx *)job->alm[i])[ofs+l*stride]*job->norm_l[l];
      }
    }
  else
    SET_ARRAY(job->almtmp,job->ntrans*job->nalm*job->ainfo->mval[mi],
              job->ntrans*job->nalm*(lmax+1),0.);
  }

static void almtmp2alm (hpsharp_job *job, int lmax, int mi)
  {
  if (job->type != SHARP_MAP2ALM) return;
  ptrdiff_t ofs=job->ainfo->mvstart[mi];
  int stride=job->ainfo->stride;
  if (job->spin==0)
    {
    if (job->flags&SHARP_DP)
      for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
        for (int i=0;i<job->ntrans*job->nalm;++i)
          ((dcmplx *)job->alm[i])[ofs+l*stride] +=
            job->almtmp[job->ntrans*job->nalm*l+i];
    else
      for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
        for (int i=0;i<job->ntrans*job->nalm;++i)
          ((fcmplx *)job->alm[i])[ofs+l*stride] +=
            (fcmplx)(job->almtmp[job->ntrans*job->nalm*l+i]);
    }
  else
    {
    if (job->flags&SHARP_DP)
      for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
        for (int i=0;i<job->ntrans*job->nalm;++i)
          ((dcmplx *)job->alm[i])[ofs+l*stride] +=
            job->almtmp[job->ntrans*job->nalm*l+i]*job->norm_l[l];
    else
      for (int l=job->ainfo->mval[mi]; l<=lmax; ++l)
        for (int i=0;i<job->ntrans*job->nalm;++i)
          ((fcmplx *)job->alm[i])[ofs+l*stride] +=
            (fcmplx)(job->almtmp[job->ntrans*job->nalm*l+i]*job->norm_l[l]);
    }
  }

static void phase2map (hpsharp_job *job, int mmax, int llim, int ulim)
  {
  if (job->type == SHARP_MAP2ALM) return;
  int pstride = 2*job->ntrans*job->nmaps;
#pragma omp parallel
{
  ringhelper helper;
  ringhelper_init(&helper);
#pragma omp for schedule(dynamic,1)
  for (int ith=llim; ith<ulim; ++ith)
    {
    int dim2 = pstride*(ith-llim)*(mmax+1);
    for (int i=0; i<job->ntrans*job->nmaps; ++i)
      ringhelper_phase2pair(&helper,mmax,&job->phase[dim2+2*i],
        &job->phase[dim2+2*i+1],pstride,&job->ginfo->pair[ith],job->map[i],
        job->flags);
    }
  ringhelper_destroy(&helper);
} /* end of parallel region */
  }

static void hpsharp_execute_job (hpsharp_job *job)
  {
  double timer=wallTime();
  job->opcnt=0;
  int lmax = job->ainfo->lmax,
      mmax=hpsharp_get_mmax(job->ainfo->mval, job->ainfo->nm);

  job->norm_l = (job->type==SHARP_ALM2MAP_DERIV1) ?
     hpsharp_Ylmgen_get_d1norm (lmax) :
     hpsharp_Ylmgen_get_norm (lmax, job->spin);

/* clear output arrays if requested */
  init_output (job);

  int nchunks, chunksize;
  get_chunk_info(job->ginfo->npairs,(job->flags&SHARP_NVMAX)*VLEN,&nchunks,
    &chunksize);
  alloc_phase (job,mmax+1,chunksize);

/* chunk loop */
  for (int chunk=0; chunk<nchunks; ++chunk)
    {
    int llim=chunk*chunksize, ulim=IMIN(llim+chunksize,job->ginfo->npairs);
    int *ispair = RALLOC(int,ulim-llim);
    double *cth = RALLOC(double,ulim-llim), *sth = RALLOC(double,ulim-llim);
    idxhelper *stmp = RALLOC(idxhelper,ulim-llim);
    for (int i=0; i<ulim-llim; ++i)
      {
      ispair[i] = job->ginfo->pair[i+llim].r2.nph>0;
      cth[i] = job->ginfo->pair[i+llim].r1.cth;
      sth[i] = job->ginfo->pair[i+llim].r1.sth;
      stmp[i].s=sth[i];
      stmp[i].i=i;
      }
    qsort (stmp,ulim-llim,sizeof(idxhelper),idx_compare);
    int *idx = RALLOC(int,ulim-llim);
    for (int i=0; i<ulim-llim; ++i)
      idx[i]=stmp[i].i;
    DEALLOC(stmp);

/* map->phase where necessary */
    map2phase (job, mmax, llim, ulim);

#pragma omp parallel
{
    hpsharp_job ljob = *job;
    ljob.opcnt=0;
    hpsharp_Ylmgen_C generator;
    hpsharp_Ylmgen_init (&generator,lmax,mmax,ljob.spin);
    alloc_almtmp(&ljob,lmax);

#pragma omp for schedule(dynamic,1)
    for (int mi=0; mi<job->ainfo->nm; ++mi)
      {
/* alm->alm_tmp where necessary */
      alm2almtmp (&ljob, lmax, mi);

      hpsharp_inner_loop (&ljob, ispair, cth, sth, llim, ulim, &generator, mi, idx);

/* alm_tmp->alm where necessary */
      almtmp2alm (&ljob, lmax, mi);
      }

    hpsharp_Ylmgen_destroy(&generator);
    dealloc_almtmp(&ljob);

#pragma omp critical
    job->opcnt+=ljob.opcnt;
} /* end of parallel region */

/* phase->map where necessary */
    phase2map (job, mmax, llim, ulim);

    DEALLOC(ispair);
    DEALLOC(cth);
    DEALLOC(sth);
    DEALLOC(idx);
    } /* end of chunk loop */

  DEALLOC(job->norm_l);
  dealloc_phase (job);
  job->time=wallTime()-timer;
  }

static void hpsharp_build_job_common (hpsharp_job *job, hpsharp_jobtype type,
  int spin, void *alm, void *map, const hpsharp_geom_info *geom_info,
  const hpsharp_alm_info *alm_info, int ntrans, int flags)
  {
  UTIL_ASSERT((ntrans>0)&&(ntrans<=SHARP_MAXTRANS),
    "bad number of simultaneous transforms");
  if (type==SHARP_ALM2MAP_DERIV1) spin=1;
  if (type==SHARP_MAP2ALM) flags|=SHARP_USE_WEIGHTS;
  if (type==SHARP_Yt) type=SHARP_MAP2ALM;
  if (type==SHARP_WY) { type=SHARP_ALM2MAP; flags|=SHARP_USE_WEIGHTS; }

  UTIL_ASSERT((spin>=0)&&(spin<=30), "bad spin");
  job->type = type;
  job->spin = spin;
  job->norm_l = NULL;
  job->nmaps = (type==SHARP_ALM2MAP_DERIV1) ? 2 : ((spin>0) ? 2 : 1);
  job->nalm = (type==SHARP_ALM2MAP_DERIV1) ? 1 : ((spin>0) ? 2 : 1);
  job->ginfo = geom_info;
  job->ainfo = alm_info;
  job->flags = flags;
  if ((job->flags&SHARP_NVMAX)==0)
    job->flags|=hpsharp_nv_oracle (type, spin, ntrans);
  job->time = 0.;
  job->opcnt = 0;
  job->ntrans = ntrans;
  job->alm=alm;
  job->map=map;
  }

void hpsharp_execute (hpsharp_jobtype type, int spin, void *alm, void *map,
  const hpsharp_geom_info *geom_info, const hpsharp_alm_info *alm_info, int ntrans,
  int flags, double *time, unsigned long long *opcnt)
  {
  hpsharp_job job;
  hpsharp_build_job_common (&job, type, spin, alm, map, geom_info, alm_info,
    ntrans, flags);

  hpsharp_execute_job (&job);
  if (time!=NULL) *time = job.time;
  if (opcnt!=NULL) *opcnt = job.opcnt;
  }

void hpsharp_set_chunksize_min(int new_chunksize_min)
  { chunksize_min=new_chunksize_min; }
void hpsharp_set_nchunks_max(int new_nchunks_max)
  { nchunks_max=new_nchunks_max; }

int hpsharp_get_nv_max (void)
{ return 6; }

static int hpsharp_oracle (hpsharp_jobtype type, int spin, int ntrans)
  {
  int lmax=127;
  int mmax=(lmax+1)/2;
  int nrings=(lmax+1)/4;
  int ppring=1;

  spin = (spin!=0) ? 2 : 0;

  ptrdiff_t npix=(ptrdiff_t)nrings*ppring;
  hpsharp_geom_info *tinfo;
  hpsharp_make_gauss_geom_info (nrings, ppring, 0., 1, ppring, &tinfo);

  ptrdiff_t nalms = ((mmax+1)*(mmax+2))/2 + (mmax+1)*(lmax-mmax);
  int ncomp = ntrans*((spin==0) ? 1 : 2);

  double **map;
  ALLOC2D(map,double,ncomp,npix);
  SET_ARRAY(map[0],0,npix*ncomp,0.);

  hpsharp_alm_info *alms;
  hpsharp_make_triangular_alm_info(lmax,mmax,1,&alms);

  dcmplx **alm;
  ALLOC2D(alm,dcmplx,ncomp,nalms);
  SET_ARRAY(alm[0],0,nalms*ncomp,0.);

  double time=1e30;
  int nvbest=-1;

  for (int nv=1; nv<=hpsharp_get_nv_max(); ++nv)
    {
    double time_acc=0.;
    double jtime;
    int ntries=0;
    do
      {
      hpsharp_execute(type,spin,&alm[0],&map[0],tinfo,alms,ntrans,nv|SHARP_DP,
        &jtime,NULL);

      if (jtime<time) { time=jtime; nvbest=nv; }
      time_acc+=jtime;
      ++ntries;
      }
    while ((time_acc<0.02)&&(ntries<2));
    }

  DEALLOC2D(map);
  DEALLOC2D(alm);

  hpsharp_destroy_alm_info(alms);
  hpsharp_destroy_geom_info(tinfo);
  return nvbest;
  }

int hpsharp_nv_oracle (hpsharp_jobtype type, int spin, int ntrans)
  {
  static const int maxtr = 6;
  static int nv_opt[6][2][3] = {
    {{0,0,0},{0,0,0}},
    {{0,0,0},{0,0,0}},
    {{0,0,0},{0,0,0}},
    {{0,0,0},{0,0,0}},
    {{0,0,0},{0,0,0}},
    {{0,0,0},{0,0,0}} };

  if (type==SHARP_ALM2MAP_DERIV1) spin=1;
  UTIL_ASSERT((ntrans>0),"bad number of simultaneous transforms");
  UTIL_ASSERT((spin>=0)&&(spin<=30), "bad spin");
  ntrans=IMIN(ntrans,maxtr);

  if (nv_opt[ntrans-1][spin!=0][type]==0)
    nv_opt[ntrans-1][spin!=0][type]=hpsharp_oracle(type,spin,ntrans);
  return nv_opt[ntrans-1][spin!=0][type];
  }

#ifdef USE_MPI
#include "hpsharp_mpi.c"
#endif
