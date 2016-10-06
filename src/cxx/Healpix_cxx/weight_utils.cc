/*
 *  This file is part of Healpix_cxx.
 *
 *  Healpix_cxx is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Healpix_cxx is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Healpix_cxx; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  For more information about HEALPix, see http://healpix.sourceforge.net
 */

/*
 *  Healpix_cxx is being developed at the Max-Planck-Institut fuer Astrophysik
 *  and financially supported by the Deutsches Zentrum fuer Luft- und Raumfahrt
 *  (DLR).
 */

/*! \file weight_utils.cc
 *
 *  Functionality for computing ring weights and full map weights
 *
 *  Helpful literature:
 *  Graef, Kunis, Potts: On the computation of nonnegative quadrature weights
 *  on the sphere
 *    (https://www-user.tu-chemnitz.de/~potts/paper/quadgewS2.pdf)
 *  Lambers: Minimum Norm Solutions of Underdetermined Systems
 *    (http://www.math.usm.edu/lambers/mat419/lecture15.pdf)
 *  Shewchuk: An Introduction to the Conjugate Gradient Method Without the
 *  Agonizing Pain
 *    (https://www.cs.cmu.edu/~quake-papers/painless-conjugate-gradient.pdf)
 *
 *  Copyright (C) 2016 Max-Planck-Society
 *  \author Martin Reinecke
 */

#include <vector>
#include <numeric>
#include <algorithm>
#include "healpix_map.h"
#include "alm_healpix_tools.h"
#include "weight_utils.h"
#include "alm.h"
#include "sharp_cxx.h"
#include "lsconstants.h"

using namespace std;

/* General considerations concerning the full weights and their a_lm
   coefficients:
   The Healpix grid geometry has several symmetries:
     - mirror symmetry with respect to phi=0
     - mirror symmetry with respect to the equator
     - invariance with respect to rotation around the z-axis by multiples of
       pi/2
   This reduces the number of pixels with distinct weights to roughly 1/16 of
   the total number of map pixels.
   The geometrical symmetries also reduce the number of a_lm coefficients
   required to describe the weight map:
     - the mirror symmetry in phi means that all a_lm are purely real-valued
     - the mirror symmetry in theta means that all a_lm with odd l are zero
     - the rotational symmetry means that all a_lm with m not a multiple of 4
       vanish as well.

   These symmetries are used when storing a_lm coefficients and weight maps to
   save memory. So far, the SHTs themselves are carried out with full maps and
   a_lm sets, because there is no libsharp support for this kind of symmetries
   yet. */

namespace {

// inner product of two vectors
double dprod(const vector<double> &a, const vector<double> &b)
  { return inner_product(a.begin(),a.end(),b.begin(),0.); }

tsize n_fullweights (int nside)
  {
  //polar region
  int nrings=nside-1;
  int nrings2=nrings>>1;
  tsize res=nrings2*(nrings2+1);
  if (nrings&1) // odd number of rings
    res+=nrings2+1;
  //equatorial region
  nrings=nside+1;
  res+=nrings*((nside+1)>>1);
  if ((nside&1)==0) // nside is even
    res+=nrings-((nrings+1)>>1);
  return res;
  }

void apply_fullweights (Healpix_Map<double> &map, const vector<double> &wgt,
  bool setwgt)
  {
  planck_assert (map.Scheme()==RING, "bad map ordering scheme");
  int nside=map.Nside();
  planck_assert(wgt.size()==n_fullweights(nside),
    "incorrect size of weight array");
  int pix=0, vpix=0;
  for (int i=0; i<2*nside; ++i)
    {
    bool shifted = (i<nside-1) || ((i+nside)&1);
    int qpix=min(nside,i+1);
    bool odd=qpix&1;
    int wpix=((qpix+1)>>1) + ((odd||shifted) ? 0 : 1);
    int psouth=map.Npix()-pix-(qpix<<2);
    for (int j=0; j<(qpix<<2); ++j)
      {
      int j4=j%qpix;
      int rpix=min(j4,qpix - (shifted ? 1:0) - j4);
      double w = wgt[vpix+rpix];
      map[pix+j] = setwgt ? w : (map[pix+j]*(1.+w));
      if (i!=2*nside-1) // everywhere except on equator
        map[psouth+j] = setwgt ? w : (map[psouth+j]*(1.+w));
      }
    pix+=qpix<<2;
    vpix+=wpix;
    }
  }

vector<double> extract_fullweights (const Healpix_Map<double> &map)
  {
  planck_assert (map.Scheme()==RING, "bad map ordering scheme");
  int nside=map.Nside();
  vector<double> res; res.reserve(n_fullweights(nside));
  int pix=0;
  for (int i=0; i<2*nside; ++i)
    {
    bool shifted = (i<nside-1) || ((i+nside)&1);
    int qpix=min(nside,i+1);
    bool odd=qpix&1;
    int wpix=((qpix+1)>>1) + ((odd||shifted) ? 0 : 1);
    for (int j=0; j<wpix; ++j)
      res.push_back(map[pix+j]);
    pix+=qpix<<2;
    }
  return res;
  }

tsize n_weightalm (int lmax, int mmax)
  {
  int nmsteps=(mmax>>2)+1;
  return nmsteps * (((lmax+2)>>1) - nmsteps+1);
  }
vector<double> extract_weightalm (const Alm<xcomplex<double> > &alm)
  {
  vector<double> res; res.reserve(n_weightalm(alm.Lmax(),alm.Mmax()));
  for (int m=0; m<=alm.Mmax(); m+=4)
    for (int l=m; l<=alm.Lmax(); l+=2)
      res.push_back(real(alm(l,m)) * ((m==0) ? 1 : sqrt(2.)));
  return res;
  }
void expand_weightalm (const vector<double> &calm, Alm<xcomplex<double> > &alm)
  {
  planck_assert(calm.size()==n_weightalm(alm.Lmax(), alm.Mmax()),
    "incorrect size of weight array");
  alm.SetToZero();
  tsize idx=0;
  for (int m=0; m<=alm.Mmax(); m+=4)
    for (int l=m; l<=alm.Lmax(); l+=2)
      alm(l,m) = calm[idx++] * ((m==0) ? 1 : sqrt(0.5));
  }

class STS_hpwgt
  {
  private:
    int lmax, mmax, nside;

  public:
    using vectype=vector<double>;
    STS_hpwgt (int lmax_, int mmax_, int nside_)
      : lmax(lmax_), mmax(mmax_), nside(nside_)
      { planck_assert((lmax&1)==0,"lmax must be even"); }
    vectype S (const vectype &x) const
      {
      Alm<xcomplex<double> > ta(lmax,mmax);
      expand_weightalm(x,ta);
      Healpix_Map<double> tm(nside,RING, SET_NSIDE);
      alm2map(ta,tm);
      return extract_fullweights(tm);
      }
    vectype ST (const vectype &x) const
      {
      Healpix_Map<double> tm(nside,RING, SET_NSIDE);
      apply_fullweights(tm,x,true);
      Alm<xcomplex<double> > ta(lmax,mmax);
      alm2map_adjoint(tm,ta);
      return extract_weightalm(ta);
      }
    vectype apply (const vectype &x) const
      { return ST(S(x)); }
  };

class STS_hpring
  {
  private:
    int lmax, nside;
    sharp_cxxjob<double> job;

  public:
    using vectype=vector<double>;
    STS_hpring (int lmax_, int nside_)
      : lmax(lmax_), nside(nside_)
      {
      planck_assert((lmax&1)==0,"lmax must be even");
      int nring=2*nside;
      vector<double> dbl0(nring,0.),theta(nring);
      vector<int> int1(nring,1);
      vector<ptrdiff_t> ofs(nring);
      Healpix_Base base(nside, RING, SET_NSIDE);
      for (int i=0; i<nring; ++i)
        {
        ofs[i]=i;
        int idum1,idum2;
        bool bdum;
        base.get_ring_info2 (i+1, idum1, idum2, theta[i], bdum);
        }
      job.set_general_geometry (nring, int1.data(), ofs.data(),
        int1.data(), dbl0.data(), theta.data(), dbl0.data());
      job.set_triangular_alm_info (lmax, 0);
      }
    vectype S(const vectype &alm) const
      {
      planck_assert(int(alm.size())==lmax/2+1,"bad input size");
      vectype res(2*nside);
      vector<xcomplex<double> > alm2(2*alm.size()-1,0.);
      for (tsize i=0; i<alm.size(); ++i)
        alm2[2*i]=alm[i];
      job.alm2map(&alm2[0],&res[0],false);
      return res;
      }
    vectype ST(const vectype &map) const
      {
      planck_assert(int(map.size())==2*nside,"bad input size");
      vector<xcomplex<double> > alm2(lmax+1,0.);
      job.alm2map_adjoint(&map[0],&alm2[0], false);
      vectype res(lmax/2+1);
      for (tsize i=0; i<res.size(); ++i)
        res[i]=real(alm2[2*i]);
      return res;
      }
    vectype apply (const vectype &x) const
      { return ST(S(x)); }
  };

vector<double> muladd (double fct, const vector<double> &a,
  const vector<double> &b)
  {
  planck_assert(a.size()==b.size(),"types not conformable");
  vector<double> res(b);
  for (tsize i=0; i<a.size(); ++i)
    res[i] += fct*a[i];
  return res;
  }

template<typename M> void cg_solve (const M &A, typename M::vectype &x,
  const typename M::vectype &b, double epsilon, int itmax)
  {
  typename M::vectype r(b), d(b);
  double res0=0.,rr=0.;
  for (int iter=0; iter<itmax; ++iter)
    {
    if (iter%300==0) // restart iteration
      {
      r=muladd(-1.,A.apply(x),b);
      rr=dprod(r,r);
      if (rr==0.) break; // direct hit
      if (iter==0) res0=sqrt(rr);
      if (iter==0) cout << "res0: " << res0 << endl;
      d=r;
      }
    auto vtmp=A.apply(d);
    double alpha = rr/dprod(d,vtmp);
    x=muladd(alpha,d,x);
    cout << "\rIteration " << iter
         << ": residual=" << sqrt(rr)/res0 << "                    " << flush;
    if (sqrt(rr)<epsilon*res0) { cout << endl; break; } // convergence
    double rrold=rr;
    r=muladd(-alpha,vtmp,r);
    rr=dprod(r,r);
    double beta=rr/rrold;
    d=muladd(beta,d,r);
    }
  }

} // unnamed namespace

vector<double> get_fullweights(int nside, int lmax, double epsilon, int itmax)
  {
  planck_assert((lmax&1)==0,"lmax must be even");
  STS_hpwgt mat(lmax, lmax, nside);
  vector<double> x(n_weightalm(lmax,lmax),0.);
  vector<double> rhs=mat.ST(vector<double> (n_fullweights(nside),-1.));
  rhs[0]+=12*nside*nside/sqrt(4*pi);
  cg_solve(mat, x, rhs, epsilon, itmax);
  return mat.S(x);
  }

void apply_fullweights (Healpix_Map<double> &map, const vector<double> &wgt)
  { apply_fullweights (map,wgt,false); }

vector<double> get_ringweights(int nside, int lmax, double epsilon, int itmax)
  {
  planck_assert((lmax&1)==0,"lmax must be even");
  STS_hpring mat(lmax,nside);
  vector<double> nir(2*nside), x(lmax/2+1,0.);
  for (tsize ith=0; ith<nir.size(); ++ith)
    nir[ith]=8*min<int>(ith+1,nside);
  nir[2*nside-1]/=2;
  auto b=mat.ST(nir);
  for (tsize i=0; i<b.size(); ++i)
    b[i]=-b[i];
  b[0]+=12*nside*nside/sqrt(4*pi);
  cg_solve(mat, x, b, epsilon, itmax);
  auto mtmp=mat.S(x);
  for (tsize i=0; i<mtmp.size(); ++i)
    mtmp[i]/=nir[i];
  return mtmp;
  }
