/*
 *  HEALPix Java code original port for Gaia by wil.
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

 *
 */
package healpix.core;

import healpix.tools.SpatialVector;
import healpix.essentials.Pointing;

import java.text.DecimalFormat;

/**
 * An angular position theta phi
 * @author womullan
 */
public class AngularPosition extends Pointing {

	/**
	 * Default constructor
	 */
	public AngularPosition() {
	}

	/**
	 * Simple constructor init both values.
	 * @param theta in radians  [0,Pi]
	 * @param phi in radians [0,2*Pi]
	 */
	public AngularPosition(double theta, double phi) {

		this.theta = theta;
		this.phi = phi;
	}

	public AngularPosition(Pointing ptg) {
		super(ptg);
	}

	/**
	 * Theta.
	 *
	 * @return the double
	 */
	public double theta() {
		return theta;
	}

	/**
	 * Phi.
	 *
	 * @return the double
	 */
	public double phi() {
		return phi;
	}

	/**
	 * Sets the theta.
	 *
	 * @param val the new theta in radians [0,2*Pi]
	 */
	public void setTheta(double val) {
		this.theta = val;
	}

	/**
	 * Sets the phi.
	 *
	 * @param val the new phi in radians [0,Pi]
	 */
	public void setPhi(double val) {
		this.phi = val;

	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		DecimalFormat form = new DecimalFormat(" 0.000");
		return ("theta:" + form.format(theta) + " phi:" + form.format(phi));

	}

	/**
	 * Inits the.
	 *
	 * @param t the t
	 * @param phi the phi
	 */
	public void init(double t, double phi) {
		this.theta=t;
		this.phi=phi;
	}

	/**
	 * convenience conversion.
	 * @return a normalized 3D vector pointing in the same direction
	 */
	public SpatialVector getAsVector() {
		double x, y, z;
		double sth = Math.sin(theta);
		x = sth * Math.cos(phi);
		y = sth * Math.sin(phi);
		z = Math.cos(theta);
		return new SpatialVector(x, y, z);
	}
}
