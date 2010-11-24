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
/*   vec2ang.c
 *
 */

/* Standard Includes */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>


void vec2ang(double *vec, double *theta, double *phi) {

  double norm;
  double twopi=2*M_PI;

  norm   = sqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
  *theta = acos(vec[2]/norm);

  *phi = 0.0;
  if (vec[0] != 0.0 || vec[1] != 0.0) {
    *phi   = atan2(vec[1],vec[0]); /* in ]-pi, pi] */
    if (*phi < 0.0) *phi += twopi; /* in  [0, 2pi[ */
  }


}
