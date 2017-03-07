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

/*! \file sharp_core_inc.c
 *  Type-dependent code for the computational core
 *
 *  Copyright (C) 2012-2016 Max-Planck-Society
 *  \author Martin Reinecke
 */

typedef struct
  { Tv v[nvec]; } Tb;

typedef union
  { Tb b; double s[VLEN*nvec]; } Y(Tbu);

typedef struct
  { Tb r, i; } Y(Tbri);

typedef struct
  { Tb qr, qi, ur, ui; } Y(Tbqu);

typedef struct
  { double r[VLEN*nvec], i[VLEN*nvec]; } Y(Tsri);

typedef struct
  { double qr[VLEN*nvec],qi[VLEN*nvec],ur[VLEN*nvec],ui[VLEN*nvec]; } Y(Tsqu);

typedef union
  { Y(Tbri) b; Y(Tsri)s; } Y(Tburi);

typedef union
  { Y(Tbqu) b; Y(Tsqu)s; } Y(Tbuqu);

static inline Tb Y(Tbconst)(double val)
  {
  Tv v=vload(val);
  Tb res;
  for (int i=0; i<nvec; ++i) res.v[i]=v;
  return res;
  }

static inline void Y(Tbmuleq1)(Tb * restrict a, double b)
  { Tv v=vload(b); for (int i=0; i<nvec; ++i) vmuleq(a->v[i],v); }

static inline Tb Y(Tbprod)(Tb a, Tb b)
  { Tb r; for (int i=0; i<nvec; ++i) r.v[i]=vmul(a.v[i],b.v[i]); return r; }

static inline void Y(Tbmuleq)(Tb * restrict a, Tb b)
  { for (int i=0; i<nvec; ++i) vmuleq(a->v[i],b.v[i]); }

static void Y(Tbnormalize) (Tb * restrict val, Tb * restrict scale,
  double maxval)
  {
  const Tv vfmin=vload(sharp_fsmall*maxval), vfmax=vload(maxval);
  const Tv vfsmall=vload(sharp_fsmall), vfbig=vload(sharp_fbig);
  for (int i=0;i<nvec; ++i)
    {
    Tm mask = vgt(vabs(val->v[i]),vfmax);
    while (vanyTrue(mask))
      {
      vmuleq_mask(mask,val->v[i],vfsmall);
      vaddeq_mask(mask,scale->v[i],vone);
      mask = vgt(vabs(val->v[i]),vfmax);
      }
    mask = vand_mask(vlt(vabs(val->v[i]),vfmin),vne(val->v[i],vzero));
    while (vanyTrue(mask))
      {
      vmuleq_mask(mask,val->v[i],vfbig);
      vsubeq_mask(mask,scale->v[i],vone);
      mask = vand_mask(vlt(vabs(val->v[i]),vfmin),vne(val->v[i],vzero));
      }
    }
  }

static void Y(mypow) (Tb val, int npow, const double * restrict powlimit,
  Tb * restrict resd, Tb * restrict ress)
  {
  Tv vminv=vload(powlimit[npow]);
  Tm mask = vlt(vabs(val.v[0]),vminv);
  for (int i=1;i<nvec; ++i)
    mask=vor_mask(mask,vlt(vabs(val.v[i]),vminv));
  if (!vanyTrue(mask)) // no underflows possible, use quick algoritm
    {
    Tb res=Y(Tbconst)(1.);
    do
      {
      if (npow&1)
        for (int i=0; i<nvec; ++i)
          {
          vmuleq(res.v[i],val.v[i]);
          vmuleq(val.v[i],val.v[i]);
          }
      else
        for (int i=0; i<nvec; ++i)
          vmuleq(val.v[i],val.v[i]);
      }
    while(npow>>=1);
    *resd=res;
    *ress=Y(Tbconst)(0.);
    }
  else
    {
    Tb scale=Y(Tbconst)(0.), scaleint=Y(Tbconst)(0.), res=Y(Tbconst)(1.);
    Y(Tbnormalize)(&val,&scaleint,sharp_fbighalf);
    do
      {
      if (npow&1)
        {
        for (int i=0; i<nvec; ++i)
          {
          vmuleq(res.v[i],val.v[i]);
          vaddeq(scale.v[i],scaleint.v[i]);
          }
        Y(Tbnormalize)(&res,&scale,sharp_fbighalf);
        }
      for (int i=0; i<nvec; ++i)
        {
        vmuleq(val.v[i],val.v[i]);
        vaddeq(scaleint.v[i],scaleint.v[i]);
        }
      Y(Tbnormalize)(&val,&scaleint,sharp_fbighalf);
      }
    while(npow>>=1);
    *resd=res;
    *ress=scale;
    }
  }

static inline int Y(rescale) (Tb * restrict lam1, Tb * restrict lam2,
  Tb * restrict scale)
  {
  int did_scale=0;
  for (int i=0;i<nvec; ++i)
    {
    Tm mask = vgt(vabs(lam2->v[i]),vload(sharp_ftol));
    if (vanyTrue(mask))
      {
      did_scale=1;
      vmuleq_mask(mask,lam1->v[i],vload(sharp_fsmall));
      vmuleq_mask(mask,lam2->v[i],vload(sharp_fsmall));
      vaddeq_mask(mask,scale->v[i],vone);
      }
    }
  return did_scale;
  }

static inline int Y(TballLt)(Tb a,double b)
  {
  Tv vb=vload(b);
  Tm res=vlt(a.v[0],vb);
  for (int i=1; i<nvec; ++i)
    res=vand_mask(res,vlt(a.v[i],vb));
  return vallTrue(res);
  }
static inline int Y(TballGt)(Tb a,double b)
  {
  Tv vb=vload(b);
  Tm res=vgt(a.v[0],vb);
  for (int i=1; i<nvec; ++i)
    res=vand_mask(res,vgt(a.v[i],vb));
  return vallTrue(res);
  }
static inline int Y(TballGe)(Tb a,double b)
  {
  Tv vb=vload(b);
  Tm res=vge(a.v[0],vb);
  for (int i=1; i<nvec; ++i)
    res=vand_mask(res,vge(a.v[i],vb));
  return vallTrue(res);
  }

static void Y(getCorfac)(Tb scale, Tb * restrict corfac,
  const double * restrict cf)
  {
  Y(Tbu) sc, corf;
  sc.b=scale;
  for (int i=0; i<VLEN*nvec; ++i)
    corf.s[i] = (sc.s[i]<sharp_minscale) ?
      0. : cf[(int)(sc.s[i])-sharp_minscale];
  *corfac=corf.b;
  }

static void Y(iter_to_ieee) (const Tb sth, Tb cth, int *l_,
  Tb * restrict lam_1_, Tb * restrict lam_2_, Tb * restrict scale_,
  const sharp_Ylmgen_C * restrict gen)
  {
  int l=gen->m;
  Tb lam_1=Y(Tbconst)(0.), lam_2, scale;
  Y(mypow) (sth,l,gen->powlimit,&lam_2,&scale);
  Y(Tbmuleq1) (&lam_2,(gen->m&1) ? -gen->mfac[gen->m]:gen->mfac[gen->m]);
  Y(Tbnormalize)(&lam_2,&scale,sharp_ftol);

  int below_limit = Y(TballLt)(scale,sharp_limscale);
  while (below_limit)
    {
    if (l+2>gen->lmax) {*l_=gen->lmax+1;return;}
    Tv r0=vload(gen->rf[l].f[0]),r1=vload(gen->rf[l].f[1]);
    for (int i=0; i<nvec; ++i)
      lam_1.v[i] = vsub(vmul(vmul(cth.v[i],lam_2.v[i]),r0),vmul(lam_1.v[i],r1));
    r0=vload(gen->rf[l+1].f[0]); r1=vload(gen->rf[l+1].f[1]);
    for (int i=0; i<nvec; ++i)
      lam_2.v[i] = vsub(vmul(vmul(cth.v[i],lam_1.v[i]),r0),vmul(lam_2.v[i],r1));
    if (Y(rescale)(&lam_1,&lam_2,&scale))
      below_limit = Y(TballLt)(scale,sharp_limscale);
    l+=2;
    }
  *l_=l; *lam_1_=lam_1; *lam_2_=lam_2; *scale_=scale;
  }

static inline void Y(rec_step) (Tb * restrict rxp, Tb * restrict rxm,
  Tb * restrict ryp, Tb * restrict rym, const Tb cth,
  const sharp_ylmgen_dbl3 fx)
  {
  Tv fx0=vload(fx.f[0]),fx1=vload(fx.f[1]),fx2=vload(fx.f[2]);
  for (int i=0; i<nvec; ++i)
    {
    rxp->v[i] = vsub(vmul(vsub(cth.v[i],fx1),vmul(fx0,ryp->v[i])),
                vmul(fx2,rxp->v[i]));
    rxm->v[i] = vsub(vmul(vadd(cth.v[i],fx1),vmul(fx0,rym->v[i])),
                vmul(fx2,rxm->v[i]));
    }
  }

static void Y(iter_to_ieee_spin) (const Tb cth, const Tb sth, int *l_,
  Tb * rec1p_, Tb * rec1m_, Tb * rec2p_, Tb * rec2m_,
  Tb * scalep_, Tb * scalem_, const sharp_Ylmgen_C * restrict gen)
  {
  const sharp_ylmgen_dbl3 * restrict fx = gen->fx;
  Tb cth2, sth2;
  for (int i=0; i<nvec; ++i)
    {
    cth2.v[i]=vsqrt(vmul(vadd(vone,cth.v[i]),vload(0.5)));
    cth2.v[i]=vmax(cth2.v[i],vload(1e-15));
    sth2.v[i]=vsqrt(vmul(vsub(vone,cth.v[i]),vload(0.5)));
    sth2.v[i]=vmax(sth2.v[i],vload(1e-15));
    Tm mask=vlt(sth.v[i],vzero);
    Tm cmask=vand_mask(mask,vlt(cth.v[i],vzero));
    vmuleq_mask(cmask,cth2.v[i],vload(-1.));
    Tm smask=vand_mask(mask,vgt(cth.v[i],vzero));
    vmuleq_mask(smask,sth2.v[i],vload(-1.));
    }

  Tb ccp, ccps, ssp, ssps, csp, csps, scp, scps;
  Y(mypow)(cth2,gen->cosPow,gen->powlimit,&ccp,&ccps);
  Y(mypow)(sth2,gen->sinPow,gen->powlimit,&ssp,&ssps);
  Y(mypow)(cth2,gen->sinPow,gen->powlimit,&csp,&csps);
  Y(mypow)(sth2,gen->cosPow,gen->powlimit,&scp,&scps);

  Tb rec2p, rec2m, scalep, scalem;
  Tb rec1p=Y(Tbconst)(0.), rec1m=Y(Tbconst)(0.);
  Tv prefac=vload(gen->prefac[gen->m]),
     prescale=vload(gen->fscale[gen->m]);
  for (int i=0; i<nvec; ++i)
    {
    rec2p.v[i]=vmul(prefac,ccp.v[i]);
    scalep.v[i]=vadd(prescale,ccps.v[i]);
    rec2m.v[i]=vmul(prefac,csp.v[i]);
    scalem.v[i]=vadd(prescale,csps.v[i]);
    }
  Y(Tbnormalize)(&rec2m,&scalem,sharp_fbighalf);
  Y(Tbnormalize)(&rec2p,&scalep,sharp_fbighalf);
  for (int i=0; i<nvec; ++i)
    {
    rec2p.v[i]=vmul(rec2p.v[i],ssp.v[i]);
    scalep.v[i]=vadd(scalep.v[i],ssps.v[i]);
    rec2m.v[i]=vmul(rec2m.v[i],scp.v[i]);
    scalem.v[i]=vadd(scalem.v[i],scps.v[i]);
    if (gen->preMinus_p)
      rec2p.v[i]=vneg(rec2p.v[i]);
    if (gen->preMinus_m)
      rec2m.v[i]=vneg(rec2m.v[i]);
    if (gen->s&1)
      rec2p.v[i]=vneg(rec2p.v[i]);
    }
  Y(Tbnormalize)(&rec2m,&scalem,sharp_ftol);
  Y(Tbnormalize)(&rec2p,&scalep,sharp_ftol);

  int l=gen->mhi;

  int below_limit = Y(TballLt)(scalep,sharp_limscale)
                 && Y(TballLt)(scalem,sharp_limscale);
  while (below_limit)
    {
    if (l+2>gen->lmax) {*l_=gen->lmax+1;return;}
    Y(rec_step)(&rec1p,&rec1m,&rec2p,&rec2m,cth,fx[l+1]);
    Y(rec_step)(&rec2p,&rec2m,&rec1p,&rec1m,cth,fx[l+2]);
    if (Y(rescale)(&rec1p,&rec2p,&scalep) | Y(rescale)(&rec1m,&rec2m,&scalem))
      below_limit = Y(TballLt)(scalep,sharp_limscale)
                 && Y(TballLt)(scalem,sharp_limscale);
    l+=2;
    }

  *l_=l;
  *rec1p_=rec1p; *rec2p_=rec2p; *scalep_=scalep;
  *rec1m_=rec1m; *rec2m_=rec2m; *scalem_=scalem;
  }
