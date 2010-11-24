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

  double theta, phi, th1, ph1;
  long   nside;
  long  ipix, npix, dpix, ip2, ip1, ns1, pp;
  double vec[3];

  nside = 1024;
  dpix = 1;

  /* test nside->npix->nside */
  /*
  for (pp = 0; pp < 14; pp++) {
    nside = pow(2,pp) ;
    npix = nside2npix(nside);
    ns1  = npix2nside(npix);
    printf("%ld %ld %ld %ld\n",pp,nside,ns1,npix);
  }
  */  
  
  theta = 0.5;
  phi = -0.4;
  ang2vec(theta, phi, vec);
  vec2ang(vec, &th1, &ph1); 
  printf("%f %f, %f %f\n",theta,th1,phi,ph1); 

  
  theta = -0.1;
/*    theta = 5.; */
/*    ang2vec(theta, phi, vec); */
/*    printf("%f %f %f\n",vec[0],vec[1],vec[2]); */
  

/*    printf("dpix: %ld\n", dpix); */
/*    for (ipix = 0; ipix < npix; ipix +=dpix) { */
/*      pix2ang_nest(nside, ipix, &theta, &phi); */
/*      ang2pix_ring(nside, theta, phi, &ip2); */
/*      ring2nest(nside,ip2,&ip1); */
/*      if (ip1 != ipix) {printf("%ld %ld %ld %ld\n",nside,ipix,ip2,ip1);} */
/*    } */
/*    for (ipix = 0; ipix < npix; ipix +=dpix) { */
/*      pix2ang_ring(nside, ipix, &theta, &phi); */
/*      ang2pix_nest(nside, theta, phi, &ip2); */
/*      nest2ring(nside,ip2,&ip1); */
/*      if (ip1 != ipix) {printf("%ld %ld %ld %ld\n",nside,ipix,ip2,ip1);} */
/*    } */

/*    printf("%ld\n", nside); */

  /* Later */
  return 0;
}
