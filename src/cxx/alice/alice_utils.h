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

/*! \file alice_utils.h
 *  Copyright (C) 2005-2014 David Larson, Max-Planck-Society
 *  \author David Larson \author Martin Reinecke
 */

#ifndef ALICE_UTILS_H
#define ALICE_UTILS_H

#include <cmath>
#include "vec3.h"
#include "announce.h"
#include "rotmatrix.h"
#include "PolarizationHolder.h"
#include "lsconstants.h"

/*! Returns vectors north and east, given a normalized vector location
  on the unit sphere */
void get_north_east(const vec3 &location, vec3 &north, vec3 &east)
  {
  if (std::abs(location.x) + std::abs(location.y) > 0.0)
    east = vec3(-location.y,location.x,0).Norm();
  else
    east.Set(1.0,0,0);
  north = crossprod(location, east);
  }

/*! Returns a normalized direction parallel to and orthogonal to the
  the polarization given by q and u at a location on the unit sphere.
  Healpix conventions for q and u are used.  */
void get_qu_direction(const vec3 &location, double q, double u,
  vec3 &direction, vec3 &orthogonal)
  {
  vec3 north, east;
  get_north_east(location, north, east);
  double angle = safe_atan2(u, q) / 2.0;
  direction = (north * -cos(angle)) + (east * sin(angle));
  orthogonal = crossprod(location, direction);
  }

/*! Returns a new_location, an angle theta away from the old location,
   in the direction of the polarization given by q and u, and in the
   approximate direction of a right-handed rotation around the
   old_axis.  The axis around which one rotates to get to the
   new_location is also returned.  */
void get_step(const vec3 &location, double q, double u, double theta,
  const vec3 &old_axis, vec3 &new_location, vec3 &new_axis)
  {
  vec3 dummy;
  rotmatrix rot;

  get_qu_direction(location, q, u, dummy, new_axis);
  if (dotprod(new_axis, old_axis) < 0.0) new_axis.Flip();

  rot.Make_Axis_Rotation_Transform(new_axis, theta);
  rot.Transform(location, new_location);
  }

/*! Performs one Runge-Kutta second order step.  Values of Q and U
  must be correct as input, and they are updated at the end of this
  function.  */
void runge_kutta_step(const vec3 &old_location, const PolarizationHolder &ph,
  double &q, double &u, double theta, const vec3 &old_axis, vec3 &new_location,
  vec3 &new_axis, pointing &p)
  {
  // Take a half-theta step and get new values of Q and U.
  // theta*=3*sqrt(q*q+u*u);
  get_step(old_location, q, u, theta/2.0, old_axis, new_location, new_axis);
  p = pointing(new_location);
  ph.getQU(p, q, u);

  // Then take a full step, with those values of Q and U, from the
  // original location.
  get_step(old_location, q, u, theta, old_axis, new_location, new_axis);
  p = pointing(new_location);
  ph.getQU(p, q, u);
  }

/*! Second order Runge-Kutta integration on the sphere.  Given a
  starting location, a qu map of the sky, and a step size theta, this
  subroutine returns an array of pointings extending in both
  directions from the starting location.  */
void runge_kutta_2(const vec3 &location, const PolarizationHolder &ph,
  double theta, arr< pointing > &pointings)
  {
  double q, u;
  int i = pointings.size();
  pointing p(location);
  vec3 first_axis, old_axis, new_axis, new_location, dummy, old_location;

  ph.getQU(p, q, u);
  get_qu_direction(location, q, u, dummy, first_axis);
  old_axis = first_axis;
  old_location = location;

  pointings[pointings.size() / 2] = p;

  for(i = 1 + pointings.size() / 2; i < int(pointings.size()); i++)
    {
    runge_kutta_step(old_location, ph, q, u, theta, old_axis, new_location, new_axis, p);
    old_axis = new_axis;
    old_location = new_location;
    pointings[i] = p;
    }

  old_axis = -first_axis;
  old_location = location;
  for(i = -1 + pointings.size() / 2; i >= 0; i--)
    {
    runge_kutta_step(old_location, ph, q, u, theta, old_axis, new_location, new_axis, p);
    old_axis = new_axis;
    old_location = new_location;
    pointings[i] = p;
    }
  }

/*! Create a sinusoidal kernel. */
void make_kernel(arr< double > &kernel)
  {
  for(tsize i = 0; i < kernel.size(); i++)
    {
    double sinx = sin(pi * (i + 1.0) / (kernel.size() + 1.0));
    kernel[i] = sinx * sinx;
    }
  }

/*! Convolve an array with a kernel. */
void convolve(const arr< double > &kernel, const arr< double > &raw, arr< double > &convolution)
  {
  convolution.alloc(raw.size() - kernel.size() + 1);
  for(tsize i = 0; i < convolution.size(); i++)
    {
    double total = 0;
    for (tsize j = 0; j < kernel.size(); j++)
      total += kernel[j] * raw[i+j];
    convolution[i] = total;
    }
  }

#endif // ALICE_UTILS_H
