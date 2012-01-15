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
 *  For more information about HEALPix, see http://healpix.jpl.nasa.gov
 */

package healpix.newcore;

/** An angular position on the unit sphere.

    @copyright 2011 Max-Planck-Society
    @author Martin Reinecke */
public class Pointing
  {
  /** Colatitude in radians (0 is North Pole; Pi is South Pole) */
  public double theta;

  /** Longitude in radians */
  public double phi;

  /** Default constructor */
  public Pointing() {}

  public Pointing(Pointing ptg)
    { this.theta = ptg.theta; this.phi = ptg.phi; }

  /** Simple constructor initializing both values.
      @param theta in radians [0,Pi]
      @param phi in radians [0,2*Pi] */
  public Pointing(double theta, double phi)
    { this.theta = theta; this.phi = phi; }

  /** Conversion from {@link Vec3} */
  public Pointing(Vec3 vec)
    {
    theta = Math.atan2(Math.sqrt(vec.x*vec.x+vec.y*vec.y),vec.z);
    phi = Math.atan2 (vec.y,vec.x);
    if (phi<0.) phi += 2*Math.PI;
    }

  /** Conversion from {@link Zphi} */
  public Pointing (Zphi zphi)
    {
    double xy=Math.sqrt((1.-zphi.z)*(1.+zphi.z));
    theta = Math.atan2(xy,zphi.z); phi=zphi.phi;
    }
  // for some reason, the alternative below is much slower...
  //{ theta=Math.acos(zphi.z); phi=zphi.phi; }

  public String toString()
    {
    StringBuilder s = new StringBuilder();
    s.append("ptg(");s.append(theta);
    s.append(",");s.append(phi);
    s.append(")");
    return s.toString();
    }
  }
