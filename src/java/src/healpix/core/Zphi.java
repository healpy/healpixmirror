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

/** Class for storing a position on the unit sphere as a (z,phi)-tuple.
  * Copyright (C) 2011 Max-Planck-Society
  * @author Martin Reinecke */
public class Zphi
  {
  /** Cosine of the colatitude, or z component of unit vector; Range [-1;1]. */
  public double z;

  /** Longitude in radians; Range [0; 2Pi]. */
  public double phi;

  /** Default constructor */
  public Zphi() {}

  /** Creation from individual components */
  public Zphi (double z_, double phi_)
    { z=z_; phi=phi_; }

  /** Conversion from {@link Vec3} */
  public Zphi (Vec3 v)
    { z = v.z/v.length(); phi = Math.atan2(v.y,v.x); }

  /** Conversion from {@link Pointing} */
  public Zphi (Pointing ptg)
    { z = Math.cos(ptg.theta); phi=ptg.phi; }

  public String toString()
    {
    StringBuilder s = new StringBuilder();
    s.append("zphi(");s.append(z);
    s.append(",");s.append(phi);
    s.append(")");
    return s.toString();
    }
  }