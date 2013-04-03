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

package healpix.tools;
import healpix.essentials.Pointing;

/** Astronomy-related helper functions for HEALPix.
    @copyright 2012 Max-Planck-Society
    @author Martin Reinecke */
public class AstronomyHelpers
  {
  private static final double rad2deg=180./Math.PI;
  private static final double deg2rad=Math.PI/180.;

  static public Pointing raDecToPointing (double ra, double dec)
    {
    double theta=Math.max(0.,Math.min(Math.PI,0.5*Math.PI-dec*deg2rad));
    return new Pointing (theta, ra*deg2rad);
    }
  static public double[] PointingToRaDec (Pointing ptg)
    {
    double ret[] = new double[2];
    ret[0] = ptg.phi*rad2deg;
    ret[1] = (Math.PI*0.5 - ptg.theta)*rad2deg;
    return ret;
    }
  }
