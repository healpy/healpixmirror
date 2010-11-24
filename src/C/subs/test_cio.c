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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "chealpix.h"

int main (void) {
  float *map;
  long nside, npix, np, ns;
  int i;
  char file[180] = "test_output.fits" ;
  char fileforce[180] ;
  char order1[10] ;
  char order2[10] ;
  char coord[10] ;


  printf("Starting C Healpix IO test\n");

  nside = 1;
  npix = nside2npix(nside);

  map = (float *)malloc(npix*sizeof(float));

  for (i=0; i<npix; i++){
    map[i] = 2.*i;
  }

  sprintf(fileforce, "!%s",file); // leading ! to allow overwrite
  write_healpix_map( map, nside, fileforce, 1, "C");
  fprintf(stdout,"file written\n");

  np = get_fits_size(file, &ns, order1);
  fprintf(stdout,"%s %ld %ld %s\n", file, ns, np, order1);

  map = read_healpix_map(file, &ns, coord, order2);
  fprintf(stdout,"%s %s\n", coord, order2);

  fprintf(stdout,"%g %g %g %g\n",map[0],map[1],map[10],map[11]);

  free(map);

  printf("test completed\n\n");

  return 0;
}
