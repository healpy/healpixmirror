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
package healpix.plot3d.canvas3d;

import java.awt.Color;

import javax.vecmath.Color3f;

/**
 * A Sine color transform. WOM originally coded.
 * 
 * @author ejoliet
 * @version $Id: SineColorTransform.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class SineColorTransform implements ColorTransform {
	
	/** The min. */
	double min;

	/** The max. */
	double max;

	/** The a. */
	double a; // ax + b

	/** The b. */
	double b; // ax + b

	/** The scale color. */
	public double scaleColor;

	/** The blue. */
	float blue;

	/** The red. */
	float red;

	/** The green. */
	float green;

	/** The bc. */
	double bc;

	/** The gc. */
	double gc;

	/** The rc. */
	double rc;

	/** The bof. */
	double bof;

	/** The gof. */
	double gof;

	/** The rof. */
	double rof;

	/**
	 * Creates a new SineColorTransform object from minimum and max data values.
	 * 
	 * @param min
	 *            minimum value
	 * @param max
	 *            maximum value
	 */
	public SineColorTransform(double min, double max) {
		this.min = min;
		this.max = max;
		setScaleColor((double) Math.abs(max - min));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#setScaleColor(double)
	 */
	public void setScaleColor(double scaleColor) {
		this.scaleColor = scaleColor;
		bc = ColorConstants.BASE_BLUE_FACTOR * scaleColor;
		gc = ColorConstants.BASE_GREEN_FACTOR * scaleColor;
		rc = ColorConstants.BASE_RED_FACTOR * scaleColor;
		bof = ColorConstants.BASE_BLUE_OFFSET * scaleColor;
		gof = ColorConstants.BASE_GREEN_OFFSET * scaleColor;
		rof = ColorConstants.BASE_RED_OFFSET * scaleColor;
	}

	/**
	 * Get the {@link Color} of a value using the defined scale color
	 * 
	 * @param val
	 *            value
	 * @return the corresponding {@link Color}
	 */
	public Color getColor(double val) {
		blue = (float) Math.sin((val - min + bof) / bc);
		green = (float) Math.sin((val - min + gof) / gc);
		red = (float) Math.sin((val - min + rof) / rc);

		if (red < 0) {
			red = 0;
		}

		if (blue < 0) {
			blue = 0;
		}

		if (green < 0) {
			green = 0;
		}

		return new Color(red, green, blue);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#getColor(double, double,
	 *      double)
	 */
	public Color getColor(double val, double min, double max) {
		setScaleColor((double) Math.abs(max - min));

		return getColor(val);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#computeTransform()
	 */
	public void computeTransform() {
		// TODO not used yet but could be
	}

	/**
	 * Get {@link Color3f} from a value
	 * 
	 * @param val
	 *            value
	 * @return {@link Color3f}
	 */
	public Color3f getColor3f(double val) {
		getColor(val);

		return new Color3f(red, green, blue);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#getScaleColor()
	 */
	public double getScaleColor() {
		return this.scaleColor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#getMax()
	 */
	public double getMax() {
		return this.max;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see healpix.plot3d.canvas3d.ColorTransform#getMin()
	 */
	public double getMin() {
		return this.min;
	}
}
