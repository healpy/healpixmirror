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
/* nest2ring.c
 *
 */

/* Standard Includes */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Local Includes */
#include "chealpix.h"

void nest2ring( long nside, long int ipnest, long *ipring) {
  /*
    c=======================================================================
    subroutine nest2ring(nside, ipnest, ipring)
    c=======================================================================
    c     conversion from NESTED to RING pixel number
    c=======================================================================
  */
      int npix, npface, face_num, ncap, n_before;
      int ipf, ip_low, ip_trunc, ip_med, ip_hi;
      int ix, iy, jrt, jr, nr, jpt, jp, kshift, nl4;
      int ns_max=8192;

      static int pix2x[1024], pix2y[1024];
      static char nest2string_setup_done = 0;

      int jrll[12], jpll[12];// ! coordinate of the lowest corner of each face
      //      data jrll/2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4/ ! in unit of nside
      //      data jpll/1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7/ ! in unit of nside/2
      jrll[0]=2;
      jrll[1]=2;
      jrll[2]=2;
      jrll[3]=2;
      jrll[4]=3;
      jrll[5]=3;
      jrll[6]=3;
      jrll[7]=3;
      jrll[8]=4;
      jrll[9]=4;
      jrll[10]=4;
      jrll[11]=4;
      jpll[0]=1;
      jpll[1]=3;
      jpll[2]=5;
      jpll[3]=7;
      jpll[4]=0;
      jpll[5]=2;
      jpll[6]=4;
      jpll[7]=6;
      jpll[8]=1;
      jpll[9]=3;
      jpll[10]=5;
      jpll[11]=7;

      if( nside<1 || nside>ns_max ) {
	fprintf(stderr, "nside = %ld out of range\n", nside);
	exit(0);
      }
      npix = 12 * nside*nside;
      if( ipnest<0 || ipnest>npix-1 ) {
	fprintf(stderr, "ipnest out of range\n");
	exit(0);
      }

      //c     initiates the array for the pixel number -> (x,y) mapping
      if ( !nest2string_setup_done ) {
	 mk_pix2xy(pix2x,pix2y);
	 nest2string_setup_done = 1;
      }

      ncap  = 2*nside*(nside-1);// ! number of points in the North Polar cap
      nl4   = 4*nside;

      //c     finds the face, and the number in the face
      npface = nside*nside;
      //cccccc      ip = ipnest - 1         ! in {0,npix-1}

      face_num = ipnest/npface;//  ! face number in {0,11}
      ipf = (int)fmod(ipnest,npface);//  ! pixel number in the face {0,npface-1}

      //c     finds the x,y on the face (starting from the lowest corner)
      //c     from the pixel number
      ip_low = (int)fmod(ipf,1024);//       ! content of the last 10 bits
      ip_trunc =   ipf/1024;//        ! truncation of the last 10 bits
      ip_med = (int)fmod(ip_trunc,1024);//  ! content of the next 10 bits
      ip_hi  =     ip_trunc/1024;//   ! content of the high weight 10 bits

      //      ix = 1024*pix2x[ip_hi] + 32*pix2x[ip_med] + pix2x[ip_low];
      //      iy = 1024*pix2y[ip_hi] + 32*pix2y[ip_med] + pix2y[ip_low];
      ix = 1024*pix2x[ip_hi] + 32*pix2x[ip_med] + pix2x[ip_low];
      iy = 1024*pix2y[ip_hi] + 32*pix2y[ip_med] + pix2y[ip_low];
      //      cout << "ix = " << ix << " iy = " << iy << endl;

      //c     transforms this in (horizontal, vertical) coordinates
      jrt = ix + iy;//  ! 'vertical' in {0,2*(nside-1)}
      jpt = ix - iy;//  ! 'horizontal' in {-nside+1,nside-1}

      //c     computes the z coordinate on the sphere
      //      jr =  jrll[face_num+1]*nside - jrt - 1;//   ! ring number in {1,4*nside-1}
      jr =  jrll[face_num]*nside - jrt - 1;

      nr = nside;//                  ! equatorial region (the most frequent)
      n_before = ncap + nl4 * (jr - nside);
      kshift = (int)fmod(jr - nside, 2);
      if( jr<nside ) {//then     ! north pole region
         nr = jr;
         n_before = 2 * nr * (nr - 1);
         kshift = 0;
      }
      else if( jr>3*nside ) {//then ! south pole region
         nr = nl4 - jr;
         n_before = npix - 2 * (nr + 1) * nr;
         kshift = 0;
      }

      //c     computes the phi coordinate on the sphere, in [0,2Pi]
      //      jp = (jpll[face_num+1]*nr + jpt + 1 + kshift)/2;//  ! 'phi' number in the ring in {1,4*nr}
      jp = (jpll[face_num]*nr + jpt + 1 + kshift)/2;

      if( jp>nl4 ) jp = jp - nl4;
      if( jp<1 )   jp = jp + nl4;

      *ipring = n_before + jp - 1;// ! in {0, npix-1}

}
