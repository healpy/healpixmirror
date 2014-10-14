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

/*! \file alice3.cc
 *  Copyright (C) 2005-2014 David Larson, Max-Planck-Society
 *  \author David Larson \author Martin Reinecke
 */

#include <iostream>
#include "paramfile.h"
#include "healpix_map_fitsio.h"
#include "lsconstants.h"
#include "arr.h"
#include "fitshandle.h"
#include "PolarizationHolder.h"
#include "alice_utils.h"
#include "vec3.h"
#include "string_utils.h"
#include "alm.h"
#include "alm_healpix_tools.h"
#include "planck_rng.h"

using namespace std;

int lic_function(Healpix_Map<float> &hitcount, Healpix_Map<float> &texture,
  const PolarizationHolder &ph, const Healpix_Map<float> &th, int steps,
  int kernel_steps, double step_radian)
  {
  arr<double> kernel(kernel_steps), convolution, rawtexture;
  make_kernel(kernel);
  arr<pointing> curve(steps);

  texture.fill(0.);
  int num_curves=0;

  for(int i=0; i<texture.Npix(); i++)
    {
    if (hitcount[i]<1.0)
      {
      num_curves++;
      runge_kutta_2(texture.pix2vec(i), ph, step_radian, curve);
      rawtexture.alloc(curve.size());
      for (tsize i=0; i<curve.size(); i++)
        rawtexture[i] = th.interpolated_value(curve[i]);
      convolve(kernel, rawtexture, convolution);
      for (tsize j=0; j<convolution.size(); j++)
        {
        int k = texture.ang2pix(curve[j+kernel.size()/2]);
        texture[k] += convolution[j];
        hitcount[k] += 1.;
        }
      }
    }
  return num_curves;
  }

int main(int argc, const char** argv)
  {
  module_startup ("alice3", argc, argv);
  paramfile params (getParamsFromCmdline(argc,argv));

  PolarizationHolder ph;
  ph.load(params.find<string>("in"));

  int nside = params.find<int>("nside");
  int steps = params.find<int>("steps",100);
  double step_radian=arcmin2rad*params.find<double>("step_arcmin",10.);
  int kernel_steps = params.find<int>("kernel_steps",50);
  float polmin = params.find<float>("polmin",-1e30),
        polmax = params.find<float>("polmax",1e30);
  string out = params.find<string>("out");

  Healpix_Map<float> th(nside,RING,SET_NSIDE);
  if (params.param_present("texture"))
    read_Healpix_map_from_fits(params.find<string>("texture"),th);
  else
    {
    planck_rng rng;
    if (params.param_present("ell"))
      {
      int ell = params.find<int>("ell");
      if (ell<0) ell=2*nside;
      Alm<xcomplex<float> > a(ell,ell);
      a.SetToZero();
      cout << "Background texture using ell = " << ell << endl;

      a(ell,0).Set(rng.rand_gauss(),0.);
      for (int m=0; m<=ell; m++)
        a(ell,m).Set(rng.rand_gauss(),rng.rand_gauss());
      alm2map(a, th);
      }
    else
      {
      for (int i=0; i<th.Npix(); i++)
        th[i] = rng.rand_uni() - 0.5;
      }
    }

  Healpix_Map<float> hit(nside,RING,SET_NSIDE),
                     tex(nside,RING,SET_NSIDE),
                     mag(nside,RING,SET_NSIDE);

  hit.fill(0.);

  for (int i=0; i<mag.Npix(); i++)
    {
    pointing p = mag.pix2ang(i);

    mag[i] = min(polmax,max(polmin,ph.getQUMagnitude(p)));
    tex[i] = th.interpolated_value(p);
    }

  write_Healpix_map_to_fits(out+"_background.fits",tex,PLANCK_FLOAT32);

  int num_curves = lic_function(hit, tex, ph, th, steps, kernel_steps,
    step_radian);

  for (tsize i=0; i<tex.Npix(); ++i)
    tex[i]/=hit[i];
  float tmin,tmax,mmin,mmax;
  tex.minmax(tmin,tmax);
  mag.minmax(mmin,mmax);
  for (tsize i=0; i<tex.Npix(); ++i)
    {
    mag[i]*=(tex[i]-tmin);
    tex[i]=1.0-(tex[i]-tmin)/(tmax-tmin);
    }
  mag.minmax(mmin,mmax);
  for (tsize i=0; i<mag.Npix(); ++i)
    mag[i]=1.0-(mag[i]-mmin)/(mmax-mmin);
  write_Healpix_map_to_fits(out+"_texture.fits",tex,PLANCK_FLOAT32);
  write_Healpix_map_to_fits(out+"_mod_texture.fits",mag,PLANCK_FLOAT32);
  }
