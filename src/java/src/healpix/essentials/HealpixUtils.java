/*
 *  This file is part of Healpix Java.
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
 *  For more information about HEALPix, see http://healpix.sourceforge.net
 */

package healpix.essentials;

/** Collection of utility functions.

    @copyright 2011-2014 Max-Planck-Society
    @author Martin Reinecke */
public class HealpixUtils
  {
  /** Evaluates cond; if it is false, throws an Exception containing errtxt. */
  static public void check(boolean cond, String errtxt) throws Exception
    { if (!cond) throw new Exception(errtxt); }

  /** Integer base 2 logarithm.
      @param arg
      @return the largest integer {@code n} that fulfills {@code 2^n<=arg}.
      For negative arguments and zero, 0 is returned. */
  static public int ilog2(long arg)
    {
    return 63-Long.numberOfLeadingZeros(Math.max(arg,1L));
    }

  /** Integer square root.
      @param arg
      @return the integer {@code n} which fulfills {@code n^2<=arg<(n+1)^2} */
  static public int isqrt(long arg)
    {
    long res = (long)Math.sqrt(((double)arg)+0.5);
    if (arg<(1L<<50)) return (int)res;
    if (res*res>arg)
      --res;
    else if ((res+1)*(res+1)<=arg)
      ++res;
    return (int)res;
    }

  /** Computes the cosine of the angular distance between two z, phi positions
      on the unit sphere. */
  static public double cosdist_zphi (double z1, double phi1,
    double z2, double phi2)
    {
    return z1*z2+ FastMath.cos(phi1-phi2)* Math.sqrt((1.0-z1*z1)*(1.0-z2*z2));
    }
  /** Computes the cosine of the angular distance between two z, phi positions
      on the unit sphere. */
  static public double cosdist_zphi (Zphi zp1, Zphi zp2)
    { return cosdist_zphi(zp1.z,zp1.phi,zp2.z,zp2.phi); }

  /** Returns the remainder of the division {@code v1/v2}.
      The result is non-negative.
      @param v1 dividend; can be positive or negative
      @param v2 divisor; must be positive
      @return remainder of the division; positive and smaller than {@code v2} */
  static public double fmodulo (double v1, double v2)
    {
    if (v1>=0)
      return (v1<v2) ? v1 : v1%v2;
    double tmp=v1%v2+v2;
    return (tmp==v2) ? 0. : tmp;
    }

  static public boolean approx (float a, float b, float epsilon)
    { return Math.abs(a-b) < (epsilon*Math.abs(b)); }
  static public boolean approx (double a, double b, double epsilon)
    { return Math.abs(a-b) < (epsilon*Math.abs(b)); }

  static public int uniq2order (long uniq)
    { return ilog2(uniq>>>2)>>>1; }
  }
