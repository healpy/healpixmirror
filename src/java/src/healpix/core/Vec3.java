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

/** Cartesian 3-vector */
public class Vec3
  {
  public double x, y, z;

  /** Default constructor. */
  public Vec3() {}

  public Vec3 (Vec3 v)
    { x=v.x; y=v.y; z=v.z; }
  /** Creation from individual components */
  public Vec3 (double x1, double y1, double z1)
    { x=x1; y=y1; z=z1; }
  /** Conversion from {@link Pointing} */
  public Vec3 (Pointing ptg)
    {
    double sth = Math.sin(ptg.theta);
    x=sth*Math.cos(ptg.phi);
    y=sth*Math.sin(ptg.phi);
    z=Math.cos(ptg.theta);
    }
  /** Conversion from {@link Zphi} */
  public Vec3 (Zphi zphi)
    {
    double sth = Math.sqrt((1.0-zphi.z)*(1.0+zphi.z));
    x=sth*Math.cos(zphi.phi);
    y=sth*Math.sin(zphi.phi);
    z=zphi.z;
    }

  /** Vector length
      @return the length of the vector. */
  public final double length()
    { return Math.sqrt(lengthSquared()); }

  /** Squared vector length
      @return the squared length of the vector. */
  public final double lengthSquared()
    { return x*x + y*y + z*z; }

  /** Normalize the vector */
  public void normalize()
    {
    double d = 1./length();
    x *= d; y *= d; z *= d;
    }

  /** Angle between two vectors.
      @param v1 another vector
      @return the angle in radians between this vector and {@code v1};
        constrained to the range [0,PI]. */
  public final double angle(Vec3 v1)
    { return Math.atan2(cross(v1).length(), dot(v1)); }

  /** Vector cross product.
      @param v another vector
      @return the vector cross product between this vector and {@code v} */
  public Vec3 cross(Vec3 v)
    { return new Vec3(y*v.z - v.y*z, z*v.x - v.z*x, x*v.y - v.x*y); }

  /** Vector scaling.
    * @param n the scale number to be multiply to the coordinates {@code x,y,z}
    * @return the vector with coordinates multiplied by {@code n}
    */
  public Vec3 mul(double n)
    { return new Vec3(n*x, n*y, n*z); }

  /** Invert the signs of all components */
  public void flip()
    { x=-x; y=-y; z=-z; }

  /** Scale the vector by a given factor
      @param n the scale factor */
  public void scale(double n)
    { x*=n; y*=n; z*=n; }

  /** Computes the dot product of the this vector and {@code v1}.
    * @param v1 another vector
    * @return dot product */
  public final double dot(Vec3 v1)
    { return x*v1.x + y*v1.y + z*v1.z; }

  /** Vector addition
    * @param v the vector to be added
    * @return addition result */
  public Vec3 add(Vec3 v)
    { return new Vec3(x+v.x, y+v.y, z+v.z); }

  /** Vector subtraction
    * @param v the vector to be subtracted
    * @return subtraction result */
  public Vec3 sub(Vec3 v)
    { return new Vec3(x-v.x, y-v.y, z-v.z); }

  public String toString()
    {
    StringBuilder s = new StringBuilder();
    s.append("vec3(");s.append(x);
    s.append(",");s.append(y);
    s.append(",");s.append(z);
    s.append(")");
    return s.toString();
    }
  }
