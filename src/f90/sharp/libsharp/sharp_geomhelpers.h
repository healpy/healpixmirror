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

/*! \file sharp_geomhelpers.h
 *  SHARP helper function for the creation of grid geometries
 *
 *  Copyright (C) 2006-2012 Max-Planck-Society
 *  \author Martin Reinecke
 */

#ifndef PLANCK_SHARP_GEOMHELPERS_H
#define PLANCK_SHARP_GEOMHELPERS_H

#include "sharp_lowlevel.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! Creates a geometry information describing a HEALPix map with an
    Nside parameter \a nside.
    \ingroup geominfogroup */
void hpsharp_make_healpix_geom_info (int nside, int stride,
  hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing a HEALPix map with an
    Nside parameter \a nside. \a weight contains the relative ring
    weights and must have \a 2*nside entries.
    \ingroup geominfogroup */
void hpsharp_make_weighted_healpix_geom_info (int nside, int stride,
  const double *weight, hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing a Gaussian map with \a nrings
    iso-latitude rings and \a nphi pixels per ring. The azimuth of the first
    pixel in each ring is \a phi0 (in radians). The index difference between
    two adjacent pixels in an iso-latitude ring is \a stride_lon, the index
    difference between the two start pixels in consecutive iso-latitude rings
    is \a stride_lat.
    \ingroup geominfogroup */
void hpsharp_make_gauss_geom_info (int nrings, int nphi, double phi0,
  int stride_lon, int stride_lat, hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing an ECP map with \a nrings
    iso-latitude rings and \a nphi pixels per ring. The azimuth of the first
    pixel in each ring is \a phi0 (in radians). The index difference between
    two adjacent pixels in an iso-latitude ring is \a stride_lon, the index
    difference between the two start pixels in consecutive iso-latitude rings
    is \a stride_lat.
    \note The spacing of pixel centers is equidistant in colatitude and
      longitude.
    \note The sphere is pixelized in a way that the colatitude of the first ring
      is \a 0.5*(pi/nrings) and the colatitude of the last ring is
      \a pi-0.5*(pi/nrings). There are no pixel centers at the poles.
    \ingroup geominfogroup */
void hpsharp_make_ecp_geom_info (int nrings, int nphi, double phi0,
  int stride_lon, int stride_lat, hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing an ECP map with \a nrings
    iso-latitude rings and \a nphi pixels per ring. The azimuth of the first
    pixel in each ring is \a phi0 (in radians). The index difference between
    two adjacent pixels in an iso-latitude ring is \a stride_lon, the index
    difference between the two start pixels in consecutive iso-latitude rings
    is \a stride_lat.
    \note The spacing of pixel centers is equidistant in colatitude and
      longitude.
    \note The sphere is pixelized in a way that the colatitude of the first ring
      is \a 0 and that of the last ring is \a pi.
    \note This is the grid used by Huffenberger & Wandelt 2010.
    \ingroup geominfogroup */
void hpsharp_make_hw_geom_info (int nrings, int ppring, double phi0,
  int stride_lon, int stride_lat, hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing an ECP map with \a nrings
    iso-latitude rings and \a nphi pixels per ring. The azimuth of the first
    pixel in each ring is \a phi0 (in radians). The index difference between
    two adjacent pixels in an iso-latitude ring is \a stride_lon, the index
    difference between the two start pixels in consecutive iso-latitude rings
    is \a stride_lat.
    \note The spacing of pixel centers is equidistant in colatitude and
      longitude.
    \note The sphere is pixelized in a way that the colatitude of the first ring
      is \a pi/(nrings+1) and that of the last ring is \a pi-pi/(nrings+1).
    \note This is the grid used by Huffenberger & Wandelt 2010.
    \ingroup geominfogroup */
void hpsharp_make_fejer2_geom_info (int nrings, int ppring, double phi0,
  int stride_lon, int stride_lat, hpsharp_geom_info **geom_info);

/*! Creates a geometry information describing a map with \a nrings
    iso-latitude rings and \a nphi pixels per ring. The azimuth of the first
    pixel in each ring is \a phi0 (in radians). The index difference between
    two adjacent pixels in an iso-latitude ring is \a stride_lon, the index
    difference between the two start pixels in consecutive iso-latitude rings
    is \a stride_lat.
    \note The spacing of pixel centers is equidistant in colatitude and
      longitude.
    \note The sphere is pixelized in a way that the colatitude of the first ring
      is \a pi/(2*nrings-1) and that of the last ring is \a pi.
    \note This is the grid introduced by McEwen & Wiaux 2011.
    \note This function does \e not define any quadrature weights.
    \ingroup geominfogroup */
void hpsharp_make_mw_geom_info (int nrings, int ppring, double phi0,
  int stride_lon, int stride_lat, hpsharp_geom_info **geom_info);

#ifdef __cplusplus
}
#endif

#endif
