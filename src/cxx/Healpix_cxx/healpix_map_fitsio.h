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

/*! \file healpix_map_fitsio.h
 *  Copyright (C) 2003-2017 Max-Planck-Society
 *  \author Martin Reinecke
 */

#ifndef HEALPIX_MAP_FITSIO_H
#define HEALPIX_MAP_FITSIO_H

#include <string>
#include "datatypes.h"
#include "fitshandle.h"
#include "healpix_base.h"

template<typename T> class arr;
template<typename T> class Healpix_Map;

/*! \defgroup healpix_map_fitsio_group FITS-based I/O of HEALPix maps */
/*! \{ */

/*! Reads the map stored in column \a colnum of the FITS binary table
    pointed to by \a inp into \a map. */
template<typename T> void read_Healpix_map_from_fits
  (fitshandle &inp, Healpix_Map<T> &map, int colnum=1);
/*! Returns the map stored in column \a colnum of the FITS binary table
    pointed to by \a inp. */
template<typename T> Healpix_Map<T> read_Healpix_map_from_fits
  (fitshandle &inp, int colnum=1);
/*! Opens the FITS file \a filename, jumps to the HDU \a hdunum,
    and reads the column \a colnum into \a map. */
template<typename T> void read_Healpix_map_from_fits
  (const std::string &filename, Healpix_Map<T> &map, int colnum=1,
  int hdunum=2);
/*! Opens the FITS file \a filename, jumps to the HDU \a hdunum,
    and returns the map in column \a colnum. */
template<typename T> Healpix_Map<T> read_Healpix_map_from_fits
  (const std::string &filename, int colnum=1, int hdunum=2);

template<typename T> void read_Healpix_map_from_fits
  (fitshandle &inp, Healpix_Map<T> &mapT, Healpix_Map<T> &mapQ,
  Healpix_Map<T> &mapU);
template<typename T> void read_Healpix_map_from_fits
  (const std::string &filename, Healpix_Map<T> &mapT, Healpix_Map<T> &mapQ,
  Healpix_Map<T> &mapU, int hdunum=2);

/*! Inserts a new binary table into \a out, which contains one column
    of Planck type \a datatype with the name \a name, and writes all HEALPix
    specific keywords based on the information in \a base. */
void prepare_Healpix_fitsmap
  (fitshandle &out, const Healpix_Base &base, PDT datatype,
  const arr<std::string> &colname);

/*! Inserts a new binary table into \a out, which contains one column
    of Planck type \a datatype, and stores \a map into this column. */
template<typename T> void write_Healpix_map_to_fits
  (fitshandle &out, const Healpix_Map<T> &map, PDT datatype);
/*! Inserts a new binary table into \a out, which contains three columns
    of Planck type \a datatype, and stores \a mapT, \a mapQ and \a mapU
    into these columns. */
template<typename T> void write_Healpix_map_to_fits
  (fitshandle &out, const Healpix_Map<T> &mapT,
   const Healpix_Map<T> &mapQ, const Healpix_Map<T> &mapU, PDT datatype);

/*! Creates a new FITS file with the name \a outfile, with a binary table in
    HDU 2, which contains one column of Planck type \a datatype, and stores
    \a map into this column. */
template<typename T> inline void write_Healpix_map_to_fits
  (const std::string &outfile, const Healpix_Map<T> &map, PDT datatype)
  {
  fitshandle out;
  out.create (outfile);
  write_Healpix_map_to_fits (out,map,datatype);
  }
/*! Creates a new FITS file with the name \a outfile, with a binary table in
    HDU 2, which contains three columns of Planck type \a datatype, and stores
    \a mapT, \a mapQ and \a mapU into this column. */
template<typename T> inline void write_Healpix_map_to_fits
  (const std::string &outfile, const Healpix_Map<T> &mapT,
   const Healpix_Map<T> &mapQ, const Healpix_Map<T> &mapU, PDT datatype)
  {
  fitshandle out;
  out.create (outfile);
  write_Healpix_map_to_fits (out,mapT,mapQ,mapU,datatype);
  }

/*! \} */

#endif
