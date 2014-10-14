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

/*! \file PolarizationHolder.h
 *  Copyright (C) 2005-2014 David Larson, Max-Planck-Society
 *  \author David Larson \author Martin Reinecke
 */

#ifndef POLARIZATION_HOLDER
#define POLARIZATION_HOLDER

#include "healpix_map.h"
#include "healpix_map_fitsio.h"

class PolarizationHolder
  {
  private:
    Healpix_Map<float> Q, U;

  public:
    // Load a polarized fits file, with Q and U as the second
    // and third columns (the standard form).
    void load(const std::string &filename)
      {
      read_Healpix_map_from_fits(filename, Q, 2, 2);
      read_Healpix_map_from_fits(filename, U, 3, 2);
#if 0
      Healpix_Map<float> mag(Q);
      for (int i=0; i<Q.Npix(); ++i)
        mag[i]=sqrt(Q[i]*Q[i] + U[i]*U[i]);
      float mmin,mmax;
      mag.minmax(mmin,mmax);
      for (int i=0; i<Q.Npix(); ++i)
        {
        Q[i]/=mmax;
        U[i]/=mmax;
        }
#endif
      }

    // Return the polarization at some pointing.
    template<typename T> void getQU(const pointing& p, T &q, T &u) const
      {
      q = T(Q.interpolated_value(p));
      u = T(U.interpolated_value(p));
      }

    // Return the magnitude of the polarization at some pointing.
    float getQUMagnitude(const pointing& p) const
      {
      float q = Q.interpolated_value(p);
      float u = U.interpolated_value(p);
      return sqrt(q*q + u*u);
      }
  };

#endif // POLARIZATION_HOLDER
