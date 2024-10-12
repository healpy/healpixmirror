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
 *  Copyright (C) 2005-2015 David Larson, Max-Planck-Society
 *  \author David Larson \author Martin Reinecke
 */

#include <iostream>
#include "alice3.h"
#include "announce.h"
#include "paramfile.h"
#include "healpix_map_fitsio.h"
#include "lsconstants.h"
#include "arr.h"
#include "fitshandle.h"
#include "vec3.h"
#include "string_utils.h"
#include "alm.h"
#include "alm_healpix_tools.h"
#include "planck_rng.h"
#include "healpix_map.h"

using namespace std;

int main(int argc, const char** argv)
  {
  module_startup ("alice3", argc, argv);
  paramfile params (getParamsFromCmdline(argc,argv));

  Healpix_Map<double> Q, U;
  // Load a polarized fits file, with Q and U as the second
  // and third columns (the standard form).
  read_Healpix_map_from_fits(params.find<string>("in"), Q, 2, 2);
  read_Healpix_map_from_fits(params.find<string>("in"), U, 3, 2);

  int nside = params.find<int>("nside");
  int steps = params.find<int>("steps",100);
  double step_radian=arcmin2rad*params.find<double>("step_arcmin",10.);
  int kernel_steps = params.find<int>("kernel_steps",50);
  double polmin = params.find<double>("polmin",-1e30),
        polmax = params.find<double>("polmax",1e30);
  string out = params.find<string>("out");

  Healpix_Map<double> th(nside,RING,SET_NSIDE);
  if (params.param_present("texture"))
    read_Healpix_map_from_fits(params.find<string>("texture"),th);
  else
    {
    planck_rng rng(params.find<int>("rand_seed",42));
    if (params.param_present("ell"))
      {
      int ell = params.find<int>("ell");
      if (ell<0) ell=2*nside;
      Alm<xcomplex<double> > a(ell,ell);
      a.SetToZero();
      cout << "Background texture using ell = " << ell << endl;

      a(ell,0)=fcomplex(rng.rand_gauss(),0.);
      for (int m=0; m<=ell; m++)
        { a(ell,m).real(rng.rand_gauss()); a(ell,m).imag(rng.rand_gauss()); }
      alm2map(a, th);
      }
    else
      {
      for (int i=0; i<th.Npix(); i++)
        th[i] = rng.rand_uni() - 0.5;
      }
    }

  Healpix_Map<double> hit(nside,RING,SET_NSIDE),
                     tex(nside,RING,SET_NSIDE),
                     mag(nside,RING,SET_NSIDE);

  {
  PolarizationHolder ph;
  ph.Q = Q;
  ph.U = U;

  for (int i=0; i<mag.Npix(); i++)
    tex[i] = th.interpolated_value(mag.pix2ang(i));
  write_Healpix_map_to_fits(out+"_background.fits",tex,PLANCK_FLOAT32);
  }

  lic_main(Q, U, th, hit, tex, mag, steps, kernel_steps, step_radian, polmin, polmax);

  write_Healpix_map_to_fits(out+"_texture.fits",tex,PLANCK_FLOAT32);
  write_Healpix_map_to_fits(out+"_mod_texture.fits",mag,PLANCK_FLOAT32);
  }
