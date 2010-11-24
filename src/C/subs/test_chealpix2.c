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
/* test_chealpix.c 
 *
 */

/* Standard Includes */
#include <math.h>
#include <stdio.h>

/* Local Includes */
#include "chealpix.h"

int main(void) {

  double theta, phi;
  double vec[3];
  long   nside;
  long  ipix, npix, dpix, ip2, ip1;

  printf("Starting C Healpix pixel routines test\n");

  nside = 1024;
  dpix = 23;

  /* Find the number of pixels in the full map */
  npix = nside2npix(nside);
  printf("Number of pixels in full map: %ld\n", npix);
  
  printf("dpix: %ld\n", dpix);
  printf("Nest -> ang -> vec -> ang -> Ring -> Nest\n");
  for (ipix = 0; ipix < npix; ipix +=dpix) {
    pix2ang_nest(nside, ipix, &theta, &phi);
    ang2vec(theta, phi, vec);
    vec2ang(vec, &theta, &phi);
    ang2pix_ring(nside, theta, phi, &ip2);
    ring2nest(nside,ip2,&ip1);
    if (ip1 != ipix) {printf("Error: %ld %ld %ld %ld\n",nside,ipix,ip2,ip1);}
  }
  printf("Ring -> ang -> Nest -> Ring\n");
  for (ipix = 0; ipix < npix; ipix +=dpix) {
    pix2ang_ring(nside, ipix, &theta, &phi);
    ang2pix_nest(nside, theta, phi, &ip2);
    nest2ring(nside,ip2,&ip1);
    if (ip1 != ipix) {printf("Error: %ld %ld %ld %ld\n",nside,ipix,ip2,ip1);}
  }

  printf("Nest -> vec -> Ring -> Nest\n");
  for (ipix = 0; ipix < npix; ipix +=dpix) {
    pix2vec_nest(nside, ipix, vec);
    vec2pix_ring(nside, vec, &ip2);
    ring2nest(nside,ip2,&ip1);
    if (ip1 != ipix) {printf("Error: %ld %ld %ld %ld\n",nside,ipix,ip2,ip1);}
  }
  printf("Ring -> vec -> Nest -> Ring\n");
  for (ipix = 0; ipix < npix; ipix +=dpix) {
    pix2vec_ring(nside, ipix, vec);
    vec2pix_nest(nside, vec, &ip2);
    nest2ring(nside,ip2,&ip1);
    if (ip1 != ipix) {printf("Error: %ld %ld %ld %ld\n",nside,ipix,ip2,ip1);}
  }

  printf("%ld\n", nside);
  printf("test completed\n\n");

  /* Later */
  return 0;
}
