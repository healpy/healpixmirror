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

/*! \file weight_utils.h
 *
 *  Copyright (C) 2016 Max-Planck-Society
 *  \author Martin Reinecke
 */

#ifndef PLANCK_WEIGHT_UTILS_H
#define PLANCK_WEIGHT_UTILS_H

#include <vector>
#include "healpix_map.h"

void apply_fullweights (Healpix_Map<double> &map,
  const std::vector<double> &wgt);

std::vector<double> get_fullweights(int nside, int lmax, double epsilon,
  int itmax);
std::vector<double> get_ringweights(int nside, int lmax, double epsilon,
  int itmax);

#endif
