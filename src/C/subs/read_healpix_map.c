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
/* read_healpix_map.c
 *
 */

/* Standard Includes */
#include <stdlib.h>

/* Local Includes */
#include "chealpix.h"
#include "fitsio.h"

/* 
   HISTORY :
   unknown creator of read_healpix_map
   Feb 2003, EH, added coordsys and ordering
*/

float *read_healpix_map(const char *infile, long *nside, char *coordsys, char *ordering) {
  
  /* Local Declarations */
  long     naxes, *naxis, npix, npercol, irow;
  int      status, hdutype, nfound, anynul;
  float    nulval, *map;
  char     comment[FLEN_COMMENT];
  fitsfile *fptr;

  /* Initializations */
  status = 0;

  /* Open the file */
  if ( fits_open_file(&fptr, infile, READONLY, &status) ) {
    printerror( status );
    return (float *)NULL;
  }

  /* Move to the HDU */
  if ( fits_movabs_hdu(fptr, 2, &hdutype, &status) ) {
    printerror( status );
    return (float *)NULL;
  }
  if (hdutype != BINARY_TBL) {
    fprintf(stderr, "%s (%d): Extension is not binary!\n", __FILE__, __LINE__);
    return (float *)NULL;
  }

  /* Read the sizes of the array */
  if ( fits_read_key_lng(fptr, "NAXIS", &naxes, comment, &status) ) {
    printerror( status );
    return (float *)NULL;
  }
  naxis = (long *)malloc(((size_t)naxes)*sizeof(long));
  if ( fits_read_keys_lng(fptr, "NAXIS", 1, naxes, naxis, &nfound, &status) 
       || nfound != naxes ) {
    printerror( status );
    return (float *)NULL;
  }
  if ( fits_read_key_lng(fptr, "NSIDE", nside, comment, &status) ) {
    printerror(status);
    return (float *)NULL;
  }
  npix = 12*(*nside)*(*nside);
  if ( (npix%naxis[1]) != 0 ) {
    fprintf(stderr, "%s (%d): Problem with npix.\n", __FILE__, __LINE__);
    return (float *)NULL;
  }
  npercol = npix/naxis[1];

  if (fits_read_key(fptr, TSTRING, "COORDSYS",coordsys, comment, &status)) {
    fprintf(stderr, "WARNING: Could not find %s keyword in in file %s\n", 
	    "COORDSYS",infile);
    status = 0;
  }

  if (fits_read_key(fptr, TSTRING, "ORDERING", ordering, comment, &status)) {
    fprintf(stderr, "WARNING: Could not find %s keyword in in file %s\n", 
	    "ORDERING",infile);
    status = 0;
  }

  /* Read the array */
  map = (float *)malloc(((size_t)npix)*sizeof(float));
  nulval = HEALPIX_NULLVAL;
  for (irow = 0; irow < naxis[1]; irow++) {
    if ( fits_read_col(fptr, TFLOAT, 1, irow+1, 1, npercol, &nulval, 
		       &(map[irow*npercol]), &anynul, &status) ) {
      printerror(status);
      return (float *)NULL;
    }
  }

  /* Close the file */
  if ( fits_close_file(fptr, &status) ) {
    printerror( status );
    return (float *)NULL;
  }

  /* Later */
  return map;
}
