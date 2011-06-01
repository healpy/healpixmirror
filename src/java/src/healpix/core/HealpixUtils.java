/*
 * Experimental HEALPix Java code derived from the Gaia-developed Java sources
 * and the Healpix C++ library.
 *
 *  This code is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This code is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this code; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  For more information about HEALPix, see http://healpix.jpl.nasa.gov
 */

package healpix.core;

/** Collection of utility functions. */
public class HealpixUtils
  {
  /** Evaluates cond; if it is false, throws an Exception containing errtxt. */
  static public void check(boolean cond, String errtxt) throws Exception
    { if (!cond) throw new Exception(errtxt); }

  /** Integer base 2 logarithm.
      @param arg
      @return the largest integer {@code n} that fulfills {@code 2^n<=arg}. */
  static public int ilog2(long arg)
    {
    int res=0;
    while (arg > 0x0000FFFF) { res+=16; arg>>=16; }
    if (arg > 0x000000FF) { res|=8; arg>>=8; }
    if (arg > 0x0000000F) { res|=4; arg>>=4; }
    if (arg > 0x00000003) { res|=2; arg>>=2; }
    if (arg > 0x00000001) { res|=1; }
    return res;
    }

  /** Integer square root.
      @param arg
      @return the integer {@code n} which fulfills {@code n^2<=arg<(n+1)^2} */
  static public int isqrt(long arg)
    { return (int)Math.sqrt(((double)arg)+0.5); }

  /** Computes the cosine of the angular distance between two z, phi positions
      on the unit sphere. */
  static public double cosdist_zphi (double z1, double phi1,
    double z2, double phi2)
    {
    return z1*z2+ Math.cos(phi1-phi2)* Math.sqrt((1.0-z1*z1)*(1.0-z2*z2));
    }

  /** Returns the remainder of the division {@code v1/v2}.
      The result is non-negative.
      @param v1 dividend; can be positive or negative
      @param v2 divisor; must be positive
      @return remainder of the division; positive and smaller than {@code v2} */
  static public double fmodulo (double v1, double v2)
    {
    return (v1>=0) ? ((v1<v2) ? v1 : Math.IEEEremainder(v1,v2)) :
                                    (Math.IEEEremainder(v1,v2)+v2);
    }
  }
