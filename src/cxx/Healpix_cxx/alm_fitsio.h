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

/*! \file alm_fitsio.h
 *  FITS I/O for spherical harmonic coefficients
 *
 *  Copyright (C) 2003-2017 Max-Planck-Society
 *  \author Martin Reinecke
 */

#ifndef PLANCK_ALM_FITSIO_H
#define PLANCK_ALM_FITSIO_H

#include <string>
#include "xcomplex.h"
#include "datatypes.h"
#include "fitshandle.h"

template<typename T> class Alm;

/*! \defgroup alm_fitsio_group FITS-based I/O of a_lm */
/*! \{ */

/*! Returns the maximum \a l and \a m multipole moments found in the FITS HDU
    pointed to be \a inp in \a lmax and \a mmax. */
void get_almsize(fitshandle &inp, int &lmax, int &mmax);
/*! Returns the maximum \a l and \a m multipole moments found in the HDU
    \a hdunum of file \a filename in \a lmax and \a mmax. */
void get_almsize(const std::string &filename, int &lmax, int &mmax,
  int hdunum=2);
/*! Returns the maximum \a l and \a m multipole moments found in the HDUs
    2, 3 and 4 of file \a filename in \a lmax and \a mmax. */
void get_almsize_pol(const std::string &filename, int &lmax, int &mmax);

/*! Reads the a_lm of the FITS binary table pointed to by \a inp into
    \a alms. \a alms is reallocated with the parameters \a lmax and \a mmax.
    Values not present in the FITS table are set to zero; values outside
    the requested (l,m) range are ignored. */
template<typename T> void read_Alm_from_fits
  (fitshandle &inp, Alm<xcomplex<T> > &alms, int lmax, int mmax);
/*! Returns the a_lm of the FITS binary table pointed to by \a inp.
    Values not present in the FITS table are set to zero; values outside
    the requested (l,m) range are ignored. */
template<typename T> inline Alm<xcomplex<T> > read_Alm_from_fits
  (fitshandle &inp, int lmax, int mmax)
  {
  Alm<xcomplex<T> > res;
  read_Alm_from_fits (inp, res, lmax, mmax);
  return res;
  }
/*! Opens the FITS file \a filename, jumps to the HDU \a hdunum, then reads
    the a_lm from the FITS binary table there into \a alms. \a alms is
    reallocated with the parameters \a lmax and \a mmax.
    Values not present in the FITS table are set to zero; values outside
    the requested \a (l,m) range are ignored. */
template<typename T> void read_Alm_from_fits
  (const std::string &filename, Alm<xcomplex<T> > &alms,
   int lmax, int mmax, int hdunum=2);
/*! Opens the FITS file \a filename, jumps to the HDU \a hdunum, then returns
    the a_lm from the FITS binary table there.
    Values not present in the FITS table are set to zero; values outside
    the requested \a (l,m) range are ignored. */
template<typename T> inline Alm<xcomplex<T> > read_Alm_from_fits
  (const std::string &filename, int lmax, int mmax, int hdunum=2)
  {
  Alm<xcomplex<T> > res;
  read_Alm_from_fits (filename, res, lmax, mmax, hdunum);
  return res;
  }

template<typename T> inline void read_Alm_from_fits
  (const std::string &filename, Alm<xcomplex<T> > &almT,
   Alm<xcomplex<T> > &almG, Alm<xcomplex<T> > &almC,
   int lmax, int mmax, int firsthdu=2)
  {
  read_Alm_from_fits (filename, almT, lmax, mmax, firsthdu);
  read_Alm_from_fits (filename, almG, lmax, mmax, firsthdu+1);
  read_Alm_from_fits (filename, almC, lmax, mmax, firsthdu+2);
  }

/*! Inserts a new binary table into \a out, which contains three columns
    of type PLANCK_INT32, \a datatype and \a datatype, respectively.
    The data in \a alms is written into this table; values outside
    the requested (\a lmax, \a mmax) range are omitted. */
template<typename T> void write_Alm_to_fits
  (fitshandle &out, const Alm<xcomplex<T> > &alms,
   int lmax, int mmax, PDT datatype);

template<typename T> inline void write_Alm_to_fits
  (const std::string &outfile, const Alm<xcomplex<T> > &alms,
   int lmax, int mmax, PDT datatype)
  {
  fitshandle out;
  out.create(outfile);
  write_Alm_to_fits (out, alms, lmax, mmax, datatype);
  }

template<typename T> inline void write_Alm_to_fits
  (const std::string &outfile, const Alm<xcomplex<T> > &almT,
   const Alm<xcomplex<T> > &almG, const Alm<xcomplex<T> > &almC,
   int lmax, int mmax, PDT datatype)
  {
  fitshandle out;
  out.create(outfile);
  write_Alm_to_fits (out, almT, lmax, mmax, datatype);
  write_Alm_to_fits (out, almG, lmax, mmax, datatype);
  write_Alm_to_fits (out, almC, lmax, mmax, datatype);
  }

/*! Inserts a new binary table into \a out, which contains three columns
    of type PLANCK_INT32, \a datatype and \a datatype, respectively.
    The data in \a alms is written into this table; values outside
    the requested (\a lmax, \a mmax) range are omitted. Values with an absolute
    magnitude of zero are not written. */
template<typename T> void write_compressed_Alm_to_fits
  (fitshandle &out, const Alm<xcomplex<T> > &alms,
   int lmax, int mmax, PDT datatype);

/*! \} */

#endif
