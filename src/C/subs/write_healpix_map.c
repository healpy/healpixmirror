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
/* write_healpix_map.c
 *
 *
 *  write_healpix_map : routine to write a FITS binary table Planck simulations in a given coordinates system
 *
 *    signal   : signal in each pixel
 *    nside    : parameter to define Planck simulation resolution 
 *    filename : name of the HealPix file written
 *    nest     : structure type of the HealPix file (RING/NESTED)
 *    coordsys : code of the corrdinates system
 *
 *  requires cfitsio library and fitsio.h file (from the same package)
 *  http://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html
 *
 *  version 1.0, EH, July 1999
 *  version 1.1, modifications by Rosa Ruiloba (NOVELTIS), September 2002   
 *     - addition of the COORSYS keyword [write_hpfits_coordsys]
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "chealpix.h"
#include "fitsio.h"

void setCoordSysHP(char *,char *);


int write_healpix_map( float *signal, long nside,
		       const char *filename, char nest, char *coordsys) {

  /*******************************************************************/
  /* Create a binary table extension                                 */
  /*******************************************************************/
  fitsfile *fptr;       /* pointer to the FITS file, defined in fitsio.h */
  int status, hdutype;
  long firstrow, firstelem;
  
  int bitpix   =  SHORT_IMG;
  long naxis   =   0;
  long naxes[] = {0,0};
  
  int tfields   = 1;
  long nrows;
  
  char order[9];                 /* HEALPix ordering */
  char extname[] = "BINTABLE";   /* extension name */
  char *ttype[] = { "SIGNAL" };
  char *tform[] = { "1E" };
  char *tunit[] = { " " };
  char coordsys9[9];
  
  
  /* Calculate the number of pixels in the full map */
  nrows = 12L*nside*nside;
  
  /* initialize status before calling fitsio routines */
  status = 0;
  
  /* create new FITS file */
  if (fits_create_file(&fptr, filename, &status)) 
    fprintf(stderr, "%s (%d): Could not create new fits file.\n", 
	    __FILE__, __LINE__);
  
  if ( fits_create_img(fptr,  bitpix, naxis, naxes, &status) )
    fprintf(stderr, "%s (%d): Could not create new image file.\n", 
	    __FILE__, __LINE__);
  
  if ( fits_write_date(fptr, &status) )
    fprintf(stderr, "%s (%d): Could not add date.\n", 
	    __FILE__, __LINE__);
  
  /* move to 1nd HDU  */
  if ( fits_movabs_hdu(fptr, 1, &hdutype, &status) ) 
    fprintf(stderr, "%s (%d): Could not move to first HDU.\n", 
	    __FILE__, __LINE__);
  
  /* append a new empty binary table onto the FITS file */
  if ( fits_create_tbl( fptr, BINARY_TBL, nrows, tfields, ttype, tform,
			tunit, extname, &status) )
    fprintf(stderr, "%s (%d): Could not create new binary table.\n", 
	    __FILE__, __LINE__);
  
  if (fits_write_key(fptr, TSTRING, "PIXTYPE", "HEALPIX",
		     "HEALPIX Pixelisation", &status))
    fprintf(stderr, "%s (%d): Could not write PIXTYPE keyword.\n", 
	    __FILE__, __LINE__);
  
  if (nest) strcpy(order, "NESTED  ");
  else      strcpy(order, "RING    ");
  if (fits_write_key(fptr, TSTRING, "ORDERING", order, 
		     "Pixel ordering scheme, either RING or NESTED", &status))
    fprintf(stderr, "%s (%d): Could not write ORDERING keyword.\n", 
	    __FILE__, __LINE__);
  
  if (fits_write_key(fptr, TLONG, "NSIDE", &nside,
		     "Resolution parameter for HEALPIX", &status))  
    fprintf(stderr, "%s (%d): Could not write NSIDE keyword.\n", 
	    __FILE__, __LINE__);
 
  /*****NEW*************/
  setCoordSysHP(coordsys,coordsys9);
  if (fits_write_key(fptr, TSTRING, "COORDSYS", coordsys9,"Pixelisation coordinate system", &status))
    fprintf(stderr, "%s (%d): Could not write COORDSYS keyword.\n",__FILE__, __LINE__);

  /*****END**NEW*******/

  if (fits_write_comment(fptr,"           G = Galactic, E = ecliptic, C = celestial = equatorial  ", &status))
    fprintf(stderr, "%s (%d): Could not write COORDSYS explanation keyword.\n", 
	    __FILE__, __LINE__);
  
  firstrow  = 1;  /* first row in table to write   */
  firstelem = 1;  /* first element in row  (ignored in ASCII tables)  */
  
  if (fits_write_col(fptr, TFLOAT, 1, firstrow, firstelem, nrows, signal,
		     &status))
    fprintf(stderr, "%s (%d): Could not write signal.\n", __FILE__, __LINE__);

  /*fits_write_col(fptr, TLONG, 2, firstrow, firstelem, nrows, pixel,  &status);*/
  /*fits_write_col(fptr, TLONG, 3, firstrow, firstelem, nrows, n_obs,  &status);*/
  /*fits_write_col(fptr, TFLOAT, 4, firstrow, firstelem, nrows, serror,&status);*/
  
  if ( fits_close_file(fptr, &status) )       /* close the FITS file */
    fprintf(stderr, "%s (%d): Could not close file.\n", 
	    __FILE__, __LINE__);
  
  return status;
}


void setCoordSysHP(char *coordsys,char *coordsys9){
  
  strcpy(coordsys9,"C       ");
  
  if(strncmp(coordsys,"G",1)!=0 &&  strncmp(coordsys,"E",1)!=0 &&  strncmp(coordsys,"C",1)!=0 && strncmp(coordsys,"Q",1)!=0)
    fprintf(stderr, "%s (%d): System Cordinates is not correct (Galactic,Ecliptic,Celestial=Equatorial). Celestial system was set.\n", __FILE__, __LINE__);
  
  
/*    if(strncmp(coordsys,"GAL",3)==0) */
/*      strcpy(coordsys9,"G       "); */
/*    else if(strncmp(coordsys,"ECL",3)==0) */
/*      strcpy(coordsys9,"E       "); */
/*    else if(strncmp(coordsys,"EQU",3)==0 || strncmp(coordsys,"CEL",3)==0) */
/*      strcpy(coordsys9,"C       "); */

  if(strncmp(coordsys,"G",1)==0)
    strcpy(coordsys9,"G       ");
  else if(strncmp(coordsys,"E",1)==0)
    strcpy(coordsys9,"E       ");
  else if(strncmp(coordsys,"C",1)==0 || strncmp(coordsys,"Q",1)==0)
    strcpy(coordsys9,"C       ");

  

}
