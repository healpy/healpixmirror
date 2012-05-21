/*
 * HEALPix Java code supported by the Gaia project.
 * Copyright (C) 2006-2011 Gaia Data Processing and Analysis Consortium
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
package healpix.tools;
import healpix.essentials.Pointing;
import healpix.essentials.Vec3;

/**
 * The SpatialVector contains standard 3D vector with the addition that each
 * coordinate (x,y,z) is also kept in ra,dec since we expect the vector to live
 * on the surface of the unit sphere, i.e.
 *
 * <pre>
 *  2   2   2
 *  x + y + z  = 1
 * </pre>
 *
 * This is not enforced, so you can specify a vector that has not unit length.
 * If you request the ra/dec of such a vector, it will be automatically
 * normalized to length 1 and you get the ra/dec of that vector (the
 * intersection of the vector's direction with the unit sphere.
 *
 * This code comes originally from the HTM library of Peter Kunst during his
 * time at JHU.
 */

public class SpatialVector extends Vec3 {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Default constructor constructs (1,0,0), ra=0, dec=0.
	 */
	public SpatialVector() {
		x = 1;
		y = 0;
		z = 0;
	}

	/**
	 * Constructor from three coordinates
	 *
	 * @param x1
	 * @param y1
	 * @param z1
	 */
	public SpatialVector(double x1, double y1, double z1) {
		x = x1;
		y = y1;
		z = z1;
	}

	public SpatialVector(Vec3 v) {
		super(v);
	}

	/**
	 * Construct from ra/dec in degrees
	 *
	 * @param ra
	 *            RA in degrees
	 * @param dec
	 *            DEC in degrees
	 */
	public SpatialVector(double ra, double dec) {
		double cd = Math.cos(dec * Constants.cPr);
		x = Math.cos(ra * Constants.cPr) * cd;
		y = Math.sin(ra * Constants.cPr) * cd;
		z = Math.sin(dec * Constants.cPr);
	}

	/**
	 * Copy constructor - be aware this only copies x,y,z
	 *
	 * @param copy
	 *            the vector to copy
	 */
	public SpatialVector(SpatialVector copy) {
		this(copy.x(), copy.y(), copy.z());
		normalized();
	}

	/**
	 * Normalized this vector
	 */
	public void normalized() {
		double d = length();
		// zero-div may occur.
		x /= d;
		y /= d;
		z /= d;
	}

	/**
	 * Sets the ra and dec angles in degrees
	 *
	 * @param ra
	 *            right ascension angle in degrees
	 * @param dec
	 *            declination angle in degrees
	 *
	 */
	public void set(double ra, double dec) {
		double cd = Math.cos(dec * Constants.cPr);
		x = Math.cos(ra * Constants.cPr) * cd;
		y = Math.sin(ra * Constants.cPr) * cd;
		z = Math.sin(dec * Constants.cPr);
	}

	/**
	 * Returns the angle in radians between this vector and the vector
	 * parameter; the return value is constrained to the range [0,PI].
	 *
	 * @param v1
	 *            the other vector
	 * @return the angle in radians in the range [0,PI]
	 */
	public final double angle(SpatialVector v1) {
		// This method should be accurate for all angles, including 0 and pi.
		double xx = y * v1.z - z * v1.y;
		double yy = z * v1.x - x * v1.z;
		double zz = x * v1.y - y * v1.x;
		double cross = Math.sqrt(xx * xx + yy * yy + zz * zz);
		return Math.atan2(cross, dot(v1));
	}

	/**
	 * Get the coordinates in a 3 elements 1D array
	 *
	 * @return coordinates [x,y,z]
	 */
	public double[] get() {
		double ret[] = new double[3];
		ret[0] = x;
		ret[1] = y;
		ret[2] = x;
		return ret;
	}

	/**
	 * @return x
	 */
	public double x() {
		return x;
	}

	/**
	 * @return y
	 */
	public double y() {
		return y;
	}

	/**
	 * @return z
	 */
	public double z() {
		return z;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.vecmath.Tuple3d#toString()
	 */
	public String toString() {
		return this.getClass().getName() + "[" + x() + ", " + y() + ", " + z()
				+ "]";
	}

	/**
	 * vector cross product
	 *
	 * @param v
	 *            the vector to cross
	 * @return the vector cross product
	 */
	public SpatialVector cross(SpatialVector v) {

		return new SpatialVector(y() * v.z() - v.y() * z(), z() * v.x() - v.z()
				* x(), x() * v.y() - v.x() * y());
	}

	/**
	 * Compare vectors if coordinates are equals
	 *
	 * @param v
	 *            the vector to be compared with
	 * @return true if both coordinates of vectors are equal
	 */
	public boolean equal(SpatialVector v) {
		return ((x() == v.x() && y() == v.y() && z() == v.z()) ? true : false);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof SpatialVector))
			return false;
		SpatialVector other = (SpatialVector) obj;
		if (Double.doubleToLongBits(x) != Double.doubleToLongBits(other.x))
			return false;
		if (Double.doubleToLongBits(y) != Double.doubleToLongBits(other.y))
			return false;
		if (Double.doubleToLongBits(z) != Double.doubleToLongBits(other.z))
			return false;
		return true;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(x);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(y);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(z);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	/**
	 * multiply with a number
	 *
	 * @param n
	 *            the scale number to be multiply to the coordinates x,y,z
	 * @return the vector with coordinates multiplied by n
	 */
	public SpatialVector mul(double n) {
		return new SpatialVector((n * x()), (n * y()), (n * z()));
	}

	/**
	 * Computes the dot product of the this vector and vector v1.
	 *
	 * @param v1
	 *            the other vector
	 * @return dot product
	 */
	public final double dot(SpatialVector v1) {
		return x * v1.x + y * v1.y + z * v1.z;
	}

	/**
	 * vector addition
	 *
	 * @param v
	 *            the vector to be added
	 * @return vector result by addition
	 */
	public SpatialVector add(SpatialVector v) {
		return new SpatialVector(x() + v.x(), y() + v.y(), z() + v.z());
	}

	/**
	 * vector subtraction
	 *
	 * @param v
	 *            the vector to be substracted
	 * @return vector result by substraction
	 */
	public SpatialVector sub(SpatialVector v) {
		return new SpatialVector(x() - v.x(), y() - v.y(), z() - v.z());
	}

	/**
	 * Get the dec angle in degrees
	 *
	 * @return declination angle
	 */
	public double dec() {
		Pointing ptg = new Pointing(this);
		return (Math.PI*0.5 - ptg.theta) / Constants.cPr;
	}

	/**
	 * Get the ra angle in degrees
	 *
	 * @return right ascension
	 */
	public double ra() {
		Pointing ptg = new Pointing(this);
		return ptg.phi / Constants.cPr;
	}

	/**
	 * convenience function - added as it is in C++ version
	 * @param zin
	 * @param phi
	 */
    public void set_z_phi (double zin, double phi)
    {
	    double sintheta = Math.sqrt((1.0-zin)*(1.0+zin));
	    x = sintheta*Math.cos(phi);
	    y = sintheta*Math.sin(phi);
	    z = zin;
    }
};
