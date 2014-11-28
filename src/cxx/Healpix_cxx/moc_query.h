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

/*! \file moc_query.h
 *  Copyright (C) 2014 Max-Planck-Society
 *  \author Martin Reinecke
 */

#ifndef MOC_QUERY_H
#define MOC_QUERY_H

#include "vec3.h"
#include "moc.h"

enum MocQueryOp { AND,OR,XOR,NOT,NONE };

class MocQueryComponent
  {
  public:
    MocQueryOp op;
    vec3 center;
    double radius;
    MocQueryComponent(MocQueryOp op_)
      : op(op_)
      { planck_assert(op_!=NONE,"bad operator"); }
    MocQueryComponent(const vec3 &cnt, double rad)
      : op (NONE), center(cnt.Norm()), radius(rad) {}
  };

template<typename I> Moc<I> mocQuery (int order,
  const std::vector<MocQueryComponent> &comp);

template<typename I> Moc<I> mocQueryInclusive (int order, int omax,
  const std::vector<MocQueryComponent> &comp);

#endif
