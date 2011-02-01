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

public class SpatialVector {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	double x, y, z;

	/** The ra_. */
	double ra_;

	/** The dec_. */
	double dec_;

	/** The ok ra dec_. */
	boolean okRaDec_;

	/**
	 * Default constructor constructs (1,0,0), ra=0, dec=0.
	 */
	public SpatialVector() {
		x = 1;
		y = 0;
		z = 0;
		ra_ = 0;
		dec_ = 0;
		okRaDec_ = true;
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
		ra_ = 0;
		dec_ = 0;
		okRaDec_ = false;
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
		ra_ = ra;
		dec_ = dec;
		okRaDec_ = true;
		updateXYZ();
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
		updateRaDec();
	}

	/**
	 * Returns the length of this vector.
	 * 
	 * @return the length of this vector
	 */
	public final double length() {
		return Math.sqrt(lengthSquared());
	}

	/**
	 * Returns the squared length of this vector.
	 * 
	 * @return the squared length of this vector
	 */
	public final double lengthSquared() {
		return x * x + y * y + z * z;
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
		ra_ = ra;
		dec_ = dec;
		okRaDec_ = true;
		updateXYZ();
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
		// return (double)Math.acos(dot(v1)/v1.length()/v.length());
		// Numerically, near 0 and PI are very bad condition for acos.
		// In 3-space, |atan2(sin,cos)| is much stable.
		double xx = y * v1.z - z * v1.y;
		double yy = z * v1.x - x * v1.z;
		double zz = x * v1.y - y * v1.x;
		double cross = Math.sqrt(xx * xx + yy * yy + zz * zz);
		return Math.abs(Math.atan2(cross, dot(v1)));
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
		if (!okRaDec_) {
			normalized();
			updateRaDec();
		}
		return dec_;
	}

	/**
	 * Get the ra angle in degrees
	 * 
	 * @return right ascension
	 */
	public double ra() {
		if (!okRaDec_) {
			normalized();
			updateRaDec();
		}
		return ra_;
	}

	/**
	 * Update x_ y_ z_ from ra_ and dec_ variables
	 */
	protected void updateXYZ() {
		double cd = Math.cos(dec_ * Constants.cPr);
		x = Math.cos(ra_ * Constants.cPr) * cd;
		y = Math.sin(ra_ * Constants.cPr) * cd;
		z = Math.sin(dec_ * Constants.cPr);
	}

	/**
	 * Update ra_ and dec_ from x_ y_ z_ variables
	 */
	protected void updateRaDec() {
		dec_ = Math.asin(z()) / Constants.cPr; // easy.
		double cd = Math.cos(dec_ * Constants.cPr);
		if (cd > Constants.EPS || cd < -Constants.EPS)
			if (y() > Constants.EPS || y() < -Constants.EPS) {
				if (y() < 0.0)
					ra_ = 360 - Math.acos(x() / cd) / Constants.cPr;
				else
					ra_ = Math.acos(x() / cd) / Constants.cPr;
			} else {
				ra_ = (x() < 0.0 ? 180 : 0.0);
				// ra_ = (x_ < 0.0 ? -1.0 : 1.0) *
				// Math.asin(y_/cd)/Constants.cPr;
			}
		else
			ra_ = 0.0;
		okRaDec_ = true;
	}

	/**
	 * @return Right Ascencion of this vector in radians
	 */
	public double toRa() {
		double phi = 0.;
		if ((x != 0.) || (y != 0))
			phi = Math.atan2(y, x); // phi in [-pi,pi]

		if (phi < 0)
			phi += 2.0 * Math.PI; // phi in [0, 2pi]

		return phi;
	}

	/**
	 * @return Declination of this vector in radians
	 */
	public double toDe() {
		double z2 = z / length();
		double theta = Math.acos(z2);
		return Math.PI / 2 - theta;
	}
};
