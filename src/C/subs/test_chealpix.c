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
  long   nside = 8192; 
/*    long   nside = 64; */
  long  ipix, jpix, npix, dpix;
/*    float *map; */

  /* Find the number of pixels in the full map */
  npix = nside2npix(nside);
  printf("Number of pixels in full map: %ld\n", npix);
  
  dpix = npix/10000L;
  printf("dpix: %ld\n", dpix);
  for (ipix = 0; ipix < npix; ipix += dpix) {
    pix2ang_nest(nside, ipix, &theta, &phi);
    printf("%9ld %12.8f %12.8f", ipix, theta, phi);
    pix2ang_ring(nside, ipix, &theta, &phi);
    printf(" %12.8f %12.8f", theta, phi);
    ring2nest(nside, ipix, &jpix);
    printf(" %9ld", jpix);
    nest2ring(nside, ipix, &jpix);
    printf(" %9ld\n", jpix);
  }

  for (theta = 0.0; theta <= M_PI; theta += 0.1) {
    for (phi = 0.0; phi <= 2.0*M_PI; phi += 0.1) {
      ang2pix_nest(nside, theta, phi, &ipix);
      printf("%12.8f %12.8f %9ld", theta, phi, ipix);
      ang2pix_ring(nside, theta, phi, &ipix);
      printf(" %9ld\n", ipix);
    }
  }

/*    map = read_healpix_map("/home/kmg/boom/dust/SFD_i150_healpix_512.fits", &nside); */
/*    write_hpfits( map, 512, "foo.fits", 0); */
  printf("%ld\n", nside);

  /* Later */
  return 0;
}
