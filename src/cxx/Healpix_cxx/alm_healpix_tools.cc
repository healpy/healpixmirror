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

/*
 *  Copyright (C) 2003-2016 Max-Planck-Society
 *  Author: Martin Reinecke
 */

#include "alm_healpix_tools.h"
#include "alm.h"
#include "healpix_map.h"
#include "xcomplex.h"
#include "sharp_cxx.h"

using namespace std;

namespace {

void checkLmaxNside(tsize lmax, tsize nside)
  {
  if (lmax>4*nside)
    cout << "\nWARNING: map analysis requested with lmax>4*nside...\n"
            "is this really what you want?\n\n";
  }

}

template<typename T> void map2alm (const Healpix_Map<T> &map,
  Alm<xcomplex<T> > &alm, const arr<double> &weight, bool add_alm)
  {
  planck_assert (map.Scheme()==RING, "map2alm: map must be in RING scheme");
  planck_assert (int(weight.size())>=2*map.Nside(),
    "map2alm: weight array has too few entries");
  planck_assert (map.fullyDefined(),"map contains undefined pixels");
  checkLmaxNside(alm.Lmax(), map.Nside());

  sharp_cxxjob<T> job;
  job.set_weighted_Healpix_geometry (map.Nside(),&weight[0]);
  job.set_triangular_alm_info (alm.Lmax(), alm.Mmax());
  job.map2alm(&map[0], &alm(0,0), add_alm);
  }

template void map2alm (const Healpix_Map<float> &map,
  Alm<xcomplex<float> > &alm, const arr<double> &weight,
  bool add_alm);
template void map2alm (const Healpix_Map<double> &map,
  Alm<xcomplex<double> > &alm, const arr<double> &weight,
  bool add_alm);

template<typename T> void alm2map_adjoint (const Healpix_Map<T> &map,
  Alm<xcomplex<T> > &alm)
  {
  planck_assert (map.Scheme()==RING,
    "alm2map_adjoint: map must be in RING scheme");
  planck_assert (map.fullyDefined(),"map contains undefined pixels");
  checkLmaxNside(alm.Lmax(), map.Nside());

  sharp_cxxjob<T> job;
  job.set_Healpix_geometry (map.Nside());
  job.set_triangular_alm_info (alm.Lmax(), alm.Mmax());
  job.alm2map_adjoint(&map[0], &alm(0,0), false);
  }

template void alm2map_adjoint (const Healpix_Map<float> &map,
  Alm<xcomplex<float> > &alm);
template void alm2map_adjoint (const Healpix_Map<double> &map,
  Alm<xcomplex<double> > &alm);

template<typename T> void map2alm_iter (const Healpix_Map<T> &map,
  Alm<xcomplex<T> > &alm, int num_iter, const arr<double> &weight)
  {
  map2alm(map,alm,weight);
  for (int iter=1; iter<=num_iter; ++iter)
    {
    Healpix_Map<T> map2(map.Nside(),map.Scheme(),SET_NSIDE);
    alm2map(alm,map2);
    for (int m=0; m<map.Npix(); ++m)
      map2[m] = map[m]-map2[m];
    map2alm(map2,alm,weight,true);
    }
  }

template void map2alm_iter (const Healpix_Map<float> &map,
  Alm<xcomplex<float> > &alm, int num_iter,
  const arr<double> &weight);
template void map2alm_iter (const Healpix_Map<double> &map,
  Alm<xcomplex<double> > &alm, int num_iter,
  const arr<double> &weight);

template<typename T> void map2alm_iter2 (const Healpix_Map<T> &map,
  Alm<xcomplex<T> > &alm, double err_abs, double err_rel)
  {
  double x_err_abs=1./err_abs, x_err_rel=1./err_rel;
  arr<double> wgt(2*map.Nside());
  wgt.fill(1);
  Healpix_Map<T> map2(map);
  alm.SetToZero();
  while(true)
    {
    map2alm(map2,alm,wgt,true);
    alm2map(alm,map2);
    double errmeasure=0;
    for (int m=0; m<map.Npix(); ++m)
      {
      double err = abs(map[m]-map2[m]);
      double rel = (map[m]!=0) ? abs(err/map[m]) : 1e300;
      errmeasure = max(errmeasure,min(err*x_err_abs,rel*x_err_rel));
      map2[m] = map[m]-map2[m];
      }
//cout << errmeasure << endl;
    if (errmeasure<1) break;
    }
  }

template void map2alm_iter2 (const Healpix_Map<double> &map,
  Alm<xcomplex<double> > &alm, double err_abs, double err_rel);


template<typename T> void map2alm_spin
  (const Healpix_Map<T> &map1, const Healpix_Map<T> &map2,
   Alm<xcomplex<T> > &alm1, Alm<xcomplex<T> > &alm2,
   int spin, const arr<double> &weight, bool add_alm)
  {
  planck_assert (map1.Scheme()==RING,
    "map2alm_spin: maps must be in RING scheme");
  planck_assert (map1.conformable(map2),
    "map2alm_spin: maps are not conformable");
  planck_assert (alm1.conformable(alm1),
    "map2alm_spin: a_lm are not conformable");
  planck_assert (int(weight.size())>=2*map1.Nside(),
    "map2alm_spin: weight array has too few entries");
  planck_assert (map1.fullyDefined()&&map2.fullyDefined(),
    "map contains undefined pixels");
  checkLmaxNside(alm1.Lmax(), map1.Nside());

  sharp_cxxjob<T> job;
  job.set_weighted_Healpix_geometry (map1.Nside(),&weight[0]);
  job.set_triangular_alm_info (alm1.Lmax(), alm1.Mmax());
  job.map2alm_spin(&map1[0],&map2[0],&alm1(0,0),&alm2(0,0),spin,add_alm);
  }

template void map2alm_spin
  (const Healpix_Map<float> &map1, const Healpix_Map<float> &map2,
   Alm<xcomplex<float> > &alm1, Alm<xcomplex<float> > &alm2,
   int spin, const arr<double> &weight, bool add_alm);
template void map2alm_spin
  (const Healpix_Map<double> &map1, const Healpix_Map<double> &map2,
   Alm<xcomplex<double> > &alm1, Alm<xcomplex<double> > &alm2,
   int spin, const arr<double> &weight, bool add_alm);

template<typename T> void map2alm_spin_iter2
  (const Healpix_Map<T> &map1, const Healpix_Map<T> &map2,
   Alm<xcomplex<T> > &alm1, Alm<xcomplex<T> > &alm2,
   int spin, double err_abs, double err_rel)
  {
  arr<double> wgt(2*map1.Nside());
  wgt.fill(1);
  Healpix_Map<T> map1b(map1), map2b(map2);
  alm1.SetToZero(); alm2.SetToZero();
  while(true)
    {
    map2alm_spin(map1b,map2b,alm1,alm2,spin,wgt,true);
    alm2map_spin(alm1,alm2,map1b,map2b,spin);
    double errmeasure=0;
    for (int m=0; m<map1.Npix(); ++m)
      {
      double err = abs(map1[m]-map1b[m]);
      double rel = (map1[m]!=0) ? abs(err/map1[m]) : 1e300;
      errmeasure = max(errmeasure,min(err/err_abs,rel/err_rel));
      map1b[m] = map1[m]-map1b[m];
      err = abs(map2[m]-map2b[m]);
      rel = (map2[m]!=0) ? abs(err/map2[m]) : 1e300;
      errmeasure = max(errmeasure,min(err/err_abs,rel/err_rel));
      map2b[m] = map2[m]-map2b[m];
      }
//cout << errmeasure << endl;
    if (errmeasure<1) break;
    }
  }

template void map2alm_spin_iter2
  (const Healpix_Map<double> &map1, const Healpix_Map<double> &map2,
   Alm<xcomplex<double> > &alm1, Alm<xcomplex<double> > &alm2,
   int spin, double err_abs, double err_rel);

template<typename T> void map2alm_pol
  (const Healpix_Map<T> &mapT,
   const Healpix_Map<T> &mapQ,
   const Healpix_Map<T> &mapU,
   Alm<xcomplex<T> > &almT,
   Alm<xcomplex<T> > &almG,
   Alm<xcomplex<T> > &almC,
   const arr<double> &weight,
   bool add_alm)
  {
  planck_assert (mapT.Scheme()==RING,
    "map2alm_pol: maps must be in RING scheme");
  planck_assert (mapT.conformable(mapQ) && mapT.conformable(mapU),
    "map2alm_pol: maps are not conformable");
  planck_assert (almT.conformable(almG) && almT.conformable(almC),
    "map2alm_pol: a_lm are not conformable");
  planck_assert (int(weight.size())>=2*mapT.Nside(),
    "map2alm_pol: weight array has too few entries");
  planck_assert (mapT.fullyDefined()&&mapQ.fullyDefined()&&mapU.fullyDefined(),
    "map contains undefined pixels");
  checkLmaxNside(almT.Lmax(), mapT.Nside());

  sharp_cxxjob<T> job;
  job.set_weighted_Healpix_geometry (mapT.Nside(),&weight[0]);
  job.set_triangular_alm_info (almT.Lmax(), almT.Mmax());
  job.map2alm(&mapT[0], &almT(0,0), add_alm);
  job.map2alm_spin(&mapQ[0],&mapU[0],&almG(0,0),&almC(0,0),2,add_alm);
  }

template void map2alm_pol
  (const Healpix_Map<float> &mapT,
   const Healpix_Map<float> &mapQ,
   const Healpix_Map<float> &mapU,
   Alm<xcomplex<float> > &almT,
   Alm<xcomplex<float> > &almG,
   Alm<xcomplex<float> > &almC,
   const arr<double> &weight,
   bool add_alm);
template void map2alm_pol
  (const Healpix_Map<double> &mapT,
   const Healpix_Map<double> &mapQ,
   const Healpix_Map<double> &mapU,
   Alm<xcomplex<double> > &almT,
   Alm<xcomplex<double> > &almG,
   Alm<xcomplex<double> > &almC,
   const arr<double> &weight,
   bool add_alm);


template<typename T> void map2alm_pol_iter
  (const Healpix_Map<T> &mapT,
   const Healpix_Map<T> &mapQ,
   const Healpix_Map<T> &mapU,
   Alm<xcomplex<T> > &almT,
   Alm<xcomplex<T> > &almG,
   Alm<xcomplex<T> > &almC,
   int num_iter,
   const arr<double> &weight)
  {
  map2alm_pol(mapT,mapQ,mapU,almT,almG,almC,weight);
  for (int iter=1; iter<=num_iter; ++iter)
    {
    Healpix_Map<T> mapT2(mapT.Nside(),mapT.Scheme(),SET_NSIDE),
                   mapQ2(mapT.Nside(),mapT.Scheme(),SET_NSIDE),
                   mapU2(mapT.Nside(),mapT.Scheme(),SET_NSIDE);

    alm2map_pol(almT,almG,almC,mapT2,mapQ2,mapU2);
    for (int m=0; m<mapT.Npix(); ++m)
      {
      mapT2[m] = mapT[m]-mapT2[m];
      mapQ2[m] = mapQ[m]-mapQ2[m];
      mapU2[m] = mapU[m]-mapU2[m];
      }
    map2alm_pol(mapT2,mapQ2,mapU2,almT,almG,almC,weight,true);
    }
  }

template void map2alm_pol_iter
  (const Healpix_Map<float> &mapT,
   const Healpix_Map<float> &mapQ,
   const Healpix_Map<float> &mapU,
   Alm<xcomplex<float> > &almT,
   Alm<xcomplex<float> > &almG,
   Alm<xcomplex<float> > &almC,
   int num_iter,
   const arr<double> &weight);
template void map2alm_pol_iter
  (const Healpix_Map<double> &mapT,
   const Healpix_Map<double> &mapQ,
   const Healpix_Map<double> &mapU,
   Alm<xcomplex<double> > &almT,
   Alm<xcomplex<double> > &almG,
   Alm<xcomplex<double> > &almC,
   int num_iter,
   const arr<double> &weight);

template<typename T> void map2alm_pol_iter2
  (const Healpix_Map<T> &mapT,
   const Healpix_Map<T> &mapQ,
   const Healpix_Map<T> &mapU,
   Alm<xcomplex<T> > &almT,
   Alm<xcomplex<T> > &almG,
   Alm<xcomplex<T> > &almC,
   double err_abs, double err_rel)
  {
  arr<double> wgt(2*mapT.Nside());
  wgt.fill(1);
  Healpix_Map<T> mapT2(mapT), mapQ2(mapQ), mapU2(mapU);
  almT.SetToZero(); almG.SetToZero(); almC.SetToZero();
  while(true)
    {
    map2alm_pol(mapT2,mapQ2,mapU2,almT,almG,almC,wgt,true);
    alm2map_pol(almT,almG,almC,mapT2,mapQ2,mapU2);
    double errmeasure=0;
    for (int m=0; m<mapT.Npix(); ++m)
      {
      double err = abs(mapT[m]-mapT2[m]);
      double rel = (mapT[m]!=0) ? abs(err/mapT[m]) : 1e300;
      errmeasure = max(errmeasure,min(err/err_abs,rel/err_rel));
      mapT2[m] = mapT[m]-mapT2[m];
      err = abs(mapQ[m]-mapQ2[m]);
      rel = (mapQ[m]!=0) ? abs(err/mapQ[m]) : 1e300;
      errmeasure = max(errmeasure,min(err/err_abs,rel/err_rel));
      mapQ2[m] = mapQ[m]-mapQ2[m];
      err = abs(mapU[m]-mapU2[m]);
      rel = (mapU[m]!=0) ? abs(err/mapU[m]) : 1e300;
      errmeasure = max(errmeasure,min(err/err_abs,rel/err_rel));
      mapU2[m] = mapU[m]-mapU2[m];
      }
//cout << errmeasure << endl;
    if (errmeasure<1) break;
    }
  }

template void map2alm_pol_iter2
  (const Healpix_Map<double> &mapT,
   const Healpix_Map<double> &mapQ,
   const Healpix_Map<double> &mapU,
   Alm<xcomplex<double> > &almT,
   Alm<xcomplex<double> > &almG,
   Alm<xcomplex<double> > &almC,
   double err_abs, double err_rel);


template<typename T> void alm2map (const Alm<xcomplex<T> > &alm,
  Healpix_Map<T> &map)
  {
  planck_assert (map.Scheme()==RING, "alm2map: map must be in RING scheme");

  sharp_cxxjob<T> job;
  job.set_Healpix_geometry (map.Nside());
  job.set_triangular_alm_info (alm.Lmax(), alm.Mmax());
  job.alm2map(&alm(0,0), &map[0], false);
  }

template void alm2map (const Alm<xcomplex<double> > &alm,
  Healpix_Map<double> &map);
template void alm2map (const Alm<xcomplex<float> > &alm,
  Healpix_Map<float> &map);

template<typename T> void alm2map_spin
  (const Alm<xcomplex<T> > &alm1, const Alm<xcomplex<T> > &alm2,
   Healpix_Map<T> &map1, Healpix_Map<T> &map2, int spin)
  {
  planck_assert (map1.Scheme()==RING,
    "alm2map_spin: maps must be in RING scheme");
  planck_assert (map1.conformable(map2),
    "alm2map_spin: maps are not conformable");
  planck_assert (alm1.conformable(alm2),
    "alm2map_spin: a_lm are not conformable");

  sharp_cxxjob<T> job;
  job.set_Healpix_geometry (map1.Nside());
  job.set_triangular_alm_info (alm1.Lmax(), alm1.Mmax());
  job.alm2map_spin(&alm1(0,0),&alm2(0,0),&map1[0],&map2[0],spin,false);
  }

template void alm2map_spin
  (const Alm<xcomplex<double> > &alm1, const Alm<xcomplex<double> > &alm2,
   Healpix_Map<double> &map, Healpix_Map<double> &map2, int spin);
template void alm2map_spin
  (const Alm<xcomplex<float> > &alm1, const Alm<xcomplex<float> > &alm2,
   Healpix_Map<float> &map, Healpix_Map<float> &map2, int spin);


template<typename T> void alm2map_pol
  (const Alm<xcomplex<T> > &almT,
   const Alm<xcomplex<T> > &almG,
   const Alm<xcomplex<T> > &almC,
   Healpix_Map<T> &mapT,
   Healpix_Map<T> &mapQ,
   Healpix_Map<T> &mapU)
  {
  planck_assert (mapT.Scheme()==RING,
    "alm2map_pol: maps must be in RING scheme");
  planck_assert (mapT.conformable(mapQ) && mapT.conformable(mapU),
    "alm2map_pol: maps are not conformable");
  planck_assert (almT.conformable(almG) && almT.conformable(almC),
    "alm2map_pol: a_lm are not conformable");

  sharp_cxxjob<T> job;
  job.set_Healpix_geometry (mapT.Nside());
  job.set_triangular_alm_info (almT.Lmax(), almT.Mmax());
  job.alm2map(&almT(0,0), &mapT[0], false);
  job.alm2map_spin(&almG(0,0), &almC(0,0), &mapQ[0], &mapU[0], 2, false);
  }

template void alm2map_pol (const Alm<xcomplex<double> > &almT,
                           const Alm<xcomplex<double> > &almG,
                           const Alm<xcomplex<double> > &almC,
                           Healpix_Map<double> &mapT,
                           Healpix_Map<double> &mapQ,
                           Healpix_Map<double> &mapU);

template void alm2map_pol (const Alm<xcomplex<float> > &almT,
                           const Alm<xcomplex<float> > &almG,
                           const Alm<xcomplex<float> > &almC,
                           Healpix_Map<float> &mapT,
                           Healpix_Map<float> &mapQ,
                           Healpix_Map<float> &mapU);


template<typename T> void alm2map_der1
  (const Alm<xcomplex<T> > &alm,
   Healpix_Map<T> &map,
   Healpix_Map<T> &mapdth,
   Healpix_Map<T> &mapdph)
  {
  planck_assert (map.Scheme()==RING,
    "alm2map_der1: maps must be in RING scheme");
  planck_assert (map.conformable(mapdth) && map.conformable(mapdph),
    "alm2map_der1: maps are not conformable");

  sharp_cxxjob<T> job;
  job.set_Healpix_geometry (map.Nside());
  job.set_triangular_alm_info (alm.Lmax(), alm.Mmax());
  job.alm2map(&alm(0,0), &map[0], false);
  job.alm2map_der1(&alm(0,0), &mapdth[0], &mapdph[0], false);
  }

template void alm2map_der1 (const Alm<xcomplex<double> > &alm,
                            Healpix_Map<double> &map,
                            Healpix_Map<double> &map_dth,
                            Healpix_Map<double> &map_dph);

template void alm2map_der1 (const Alm<xcomplex<float> > &alm,
                            Healpix_Map<float> &map,
                            Healpix_Map<float> &map_dth,
                            Healpix_Map<float> &map_dph);
