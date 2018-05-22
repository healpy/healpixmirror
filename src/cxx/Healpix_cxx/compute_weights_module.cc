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
 *  Copyright (C) 2016-2018 Max-Planck-Society
 *  Author: Martin Reinecke
 */

#include "paramfile.h"
#include "fitshandle.h"
#include "announce.h"
#include "weight_utils.h"

using namespace std;

int compute_weights_module (int argc, const char **argv)
  {
  module_startup ("compute_weights", argc, argv);
  paramfile params (getParamsFromCmdline(argc,argv));

  planck_assert (params.param_present("outfile_ring")
              || params.param_present("outfile_full"), "no job requested");
  int nside = params.find<int>("nside");
  int nlmax = params.find<int>("nlmax");
  if (nlmax&1)
    {
    cout << "Warning: specified nlmax is odd. Reducing by 1" << endl;
    --nlmax;
    }
  double epsilon = params.find<double>("epsilon",1e-7);
  int itmax = params.find<int>("max_iter",10000);

  if (params.param_present("outfile_ring"))
    {
    double epsilon_out;
    vector<double> wgt=get_ringweights(nside,nlmax,epsilon,itmax,epsilon_out);
    fitshandle out;
    out.create (params.find<string>("outfile_ring"));
    vector<fitscolumn> cols;
    int repcount = 1;
    cols.push_back (fitscolumn ("TEMPERATURE WEIGHTS","1",repcount,
      PLANCK_FLOAT64));
    cols.push_back (fitscolumn ("Q-POLARISATION WEIGHTS","1",repcount,
      PLANCK_FLOAT64));
    cols.push_back (fitscolumn ("U-POLARISATION WEIGHTS","1",repcount,
      PLANCK_FLOAT64));
    out.insert_bintab(cols);
    out.set_key ("CREATOR",string("compute_weights"),
      "Software creating the FITS file");
    out.set_key ("NSIDE",nside,"Resolution parameter for HEALPIX");
    out.set_key ("MAX_LPOL",nlmax,"Maximum multipole l used in map synthesis");
    double val=*max_element(wgt.begin(),wgt.end());
    out.set_key("MAXVAL1",val,"maximum value of T weights");
    out.set_key("MAXVAL2",val,"maximum value of Q weights");
    out.set_key("MAXVAL3",val,"maximum value of U weights");
    val=*min_element(wgt.begin(),wgt.end());
    out.set_key("MINVAL1",val,"minimum value of T weights");
    out.set_key("MINVAL2",val,"minimum value of Q weights");
    out.set_key("MINVAL3",val,"minimum value of U weights");
    out.set_key("EPSILON",epsilon_out,"epsilon reached after minimization");
    out.write_column(1,wgt);
    out.write_column(2,wgt);
    out.write_column(3,wgt);
    }
  if (params.param_present("outfile_full"))
    {
    double epsilon_out;
    vector<double> wgt=get_fullweights(nside,nlmax,epsilon,itmax,epsilon_out);
    fitshandle out;
    out.create (params.find<string>("outfile_full"));
    vector<fitscolumn> cols;
    int repcount = 1;
    cols.push_back (fitscolumn ("COMPRESSED PIXEL WEIGHTS","1",repcount,
      PLANCK_FLOAT64));
    out.insert_bintab(cols);
    out.set_key ("CREATOR",string("compute_weights"),
      "Software creating the FITS file");
    out.set_key ("NSIDE",nside,"Resolution parameter for HEALPix");
    out.set_key ("MAX_LPOL",nlmax,"Maximum l multipole");
    double val=*max_element(wgt.begin(),wgt.end());
    out.set_key("MAXVAL1",val,"maximum value of pixel weights");
    val=*min_element(wgt.begin(),wgt.end());
    out.set_key("MINVAL1",val,"minimum value of pixel weights");
    out.set_key("EPSILON",epsilon_out,"epsilon reached after minimization");
    out.write_column(1,wgt);
    }
  return 0;
  }
