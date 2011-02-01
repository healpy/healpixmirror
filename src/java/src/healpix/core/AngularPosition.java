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
package healpix.core;

import java.text.DecimalFormat;

/**
 * An angular position theta phi
 */
public class AngularPosition {
	
	/** The theta. */
	protected double theta = 0;

	/** The phi. */
	protected double phi = 0;

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
	}
}
