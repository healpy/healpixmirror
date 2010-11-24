/* -----------------------------------------------------------------------------
 *
 *  Copyright (C) 1997-2010 Krzysztof M. Gorski, Eric Hivon,
 *                          Benjamin D. Wandelt, Anthony J. Banday, 
 *                          Matthias Bartelmann, 
 *                          Reza Ansari & Kenneth M. Ganga 
 *
 *
 *  This file is part of HEALPix.
 *
 *  HEALPix is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  HEALPix is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with HEALPix; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  For more information about HEALPix see http://healpix.jpl.nasa.gov
 *
 *----------------------------------------------------------------------------- */
/* Standard Includes */
#include <stdlib.h>

/* Local Includes */
#include "chealpix.h"
#include "fitsio.h"

/* ====================================================== */
long get_fits_size(char *filename, long *nside, char *ordering ){

  fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
  int status, hdutype;
  char comment[80];
  long obs_npix;

  status = 0;

  if ( fits_open_file(&fptr, filename, READONLY, &status) ) 
    printerror( status );

  /* ---------------------------------------- */
  if ( fits_movabs_hdu(fptr, 2, &hdutype, &status) ) /* move to 2nd HDU  */
    printerror( status );

  if (fits_read_key(fptr, TSTRING, "ORDERING", ordering, comment, &status))
    printerror(status);

  if (fits_read_key(fptr, TLONG,"NSIDE"   , nside, comment, &status))  
    printerror(status); 

  if (fits_read_key(fptr, TLONG,"OBS_NPIX", &obs_npix, comment, &status)) {
    obs_npix = 12 * (*nside) * (*nside) ;
    status = 0;
  }


  if ( fits_close_file(fptr, &status) )
    printerror( status );
  
  return obs_npix;
}
