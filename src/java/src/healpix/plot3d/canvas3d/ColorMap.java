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
 */

package healpix.plot3d.canvas3d;

import java.awt.Color;

/**
 * Creates a color map using a color transformation of data values.
 * 
 * @author ejoliet
 * @version $Id: ColorMap.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class ColorMap {
	
	/** The trans. */
	ColorTransform trans;

	/** The min. */
	double min;

	/** The max. */
	double max;

	/**
	 * Creates a new ColorMap from min and max data values and default color
	 * transformation.
	 * 
	 * @param min
	 *            minimum value
	 * @param max
	 *            maximum value
	 */
	public ColorMap(double min, double max) {
		this.min = min;
		this.max = max;
		trans = new SineColorTransform(min, max);
	}

	/**
	 * Creates a new ColorMap object from min and max data values with a color
	 * transformation as input.
	 * 
	 * @param min
	 *            minimum value
	 * @param max
	 *            maximum value
	 * @param ctrans
	 *            a color transformation
	 */
	public ColorMap(double min, double max, ColorTransform ctrans) {
		this.trans = ctrans;
		this.min = min;
		this.max = max;
	}

	/**
	 * Converts a value into a {@link Color} using the actual color
	 * tranformation, and minimum and maximum value.
	 * 
	 * @param val
	 *            value to convert into Color
	 * @return a corresponding transformed color
	 */
	public Color getColor(double val) {
		return trans.getColor(val, min, max);
	}
}
