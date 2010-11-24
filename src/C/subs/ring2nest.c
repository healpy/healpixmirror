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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Local Includes */
#include "chealpix.h"

void ring2nest( long nside, long ipring, long *ipnest) {
  /*
    c=======================================================================
    subroutine ring2nest(nside, ipring, ipnest)
    c=======================================================================
    c     conversion from RING to NESTED pixel number
    c=======================================================================
  */
  
  double fihip, hip;
  int npix, nl2, nl4, ncap, ip, iphi, ipt, ipring1;
  int     kshift, face_num, nr;
  int irn, ire, irm, irs, irt, ifm , ifp;
  int ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf;
  int ns_max=8192;
  
  static int x2pix[128], y2pix[128];
  //      common    /xy2pix/ x2pix,y2pix

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
    fprintf(stderr, "nside out of range\n");
    exit(0);
  }
  npix = 12 * nside*nside;
  if( ipring<0 || ipring>npix-1 ) {
    fprintf(stderr, "ipring out of range\n");
    exit(0);
  }
  if( x2pix[127]<=0 ) mk_xy2pix(x2pix,y2pix);
  
  nl2 = 2*nside;
  nl4 = 4*nside;
  npix = 12*nside*nside;//      ! total number of points
  ncap = 2*nside*(nside-1);// ! points in each polar cap, =0 for nside =1
  ipring1 = ipring + 1;
  
  //c     finds the ring number, the position of the ring and the face number
  if( ipring1<=ncap ) {//then
    
    hip   = ipring1/2.;
    fihip = (int)floor ( hip );
    irn   = (int)floor( sqrt( hip - sqrt(fihip) ) ) + 1;// ! counted from North pole
    iphi  = ipring1 - 2*irn*(irn - 1);
    
    kshift = 0;
    nr = irn   ;//               ! 1/4 of the number of points on the current ring
    face_num = (iphi-1) / irn;// ! in {0,3}
  }
  else if( ipring1<=nl2*(5*nside+1) ) {//then
    
    ip    = ipring1 - ncap - 1;
    irn   = (int)floor( ip / nl4 ) + nside;//               ! counted from North pole
    iphi  = (int)fmod(ip,nl4) + 1;
    
    kshift  = (int)fmod(irn+nside,2);//  ! 1 if irn+nside is odd, 0 otherwise
    nr = nside;
    ire =  irn - nside + 1;// ! in {1, 2*nside +1}
    irm =  nl2 + 2 - ire;
    ifm = (iphi - ire/2 + nside -1) / nside;// ! face boundary
    ifp = (iphi - irm/2 + nside -1) / nside;
    if( ifp==ifm ) {//then          ! faces 4 to 7
      face_num = (int)fmod(ifp,4) + 4;
    }
    else if( ifp + 1==ifm ) {//then ! (half-)faces 0 to 3
      face_num = ifp;
    }
    else if( ifp - 1==ifm ) {//then ! (half-)faces 8 to 11
      face_num = ifp + 7;
    }
  }
  else {
    
    ip    = npix - ipring1 + 1;
    hip   = ip/2.;
    fihip = floor ( hip );
    irs   = (int)floor( sqrt( hip - sqrt(fihip) ) ) + 1;//  ! counted from South pole
    iphi  = 4*irs + 1 - (ip - 2*irs*(irs-1));
    
    kshift = 0;
    nr = irs;
    irn   = nl4 - irs;
    face_num = (iphi-1) / irs + 8;// ! in {8,11}
  }
  
  //c     finds the (x,y) on the face
  //  irt =   irn  - jrll[face_num+1]*nside + 1;//       ! in {-nside+1,0}
  //  ipt = 2*iphi - jpll[face_num+1]*nr - kshift - 1;// ! in {-nside+1,nside-1}
  irt =   irn  - jrll[face_num]*nside + 1;//       ! in {-nside+1,0}
  ipt = 2*iphi - jpll[face_num]*nr - kshift - 1;


  if( ipt>=nl2 ) ipt = ipt - 8*nside;// ! for the face #4
  
  ix =  (ipt - irt ) / 2;
  iy = -(ipt + irt ) / 2;
  
  ix_low = (int)fmod(ix,128);
  ix_hi  = ix/128;
  iy_low = (int)fmod(iy,128);
  iy_hi  = iy/128;
  //  cout << "ix_low = " << ix_low << " ix_hi = " << ix_hi << endl;
  //  cout << "iy_low = " << iy_low << " iy_hi = " << iy_hi << endl;
  //  ipf =  (x2pix[ix_hi +1]+y2pix[iy_hi +1]) * (128 * 128)
  //    + (x2pix[ix_low+1]+y2pix[iy_low+1]);//        ! in {0, nside**2 - 1}
  ipf =  (x2pix[ix_hi]+y2pix[iy_hi]) * (128 * 128)
    + (x2pix[ix_low]+y2pix[iy_low]);


  //  cout << "ipf = " << ipf << endl;
  //  for( int i(0);i<128;i++ ) cout << x2pix[i] << " || " << y2pix[i] << endl;
  *ipnest = ipf + face_num* nside *nside;//   ! in {0, 12*nside**2 - 1}
  
}

