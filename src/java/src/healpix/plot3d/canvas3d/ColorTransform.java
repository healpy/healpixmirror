/*-----------------------------------------------------------------------------
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
 *-----------------------------------------------------------------------------
 */
package healpix.plot3d.canvas3d;

import java.awt.Color;

/**
 * Interface that deals with color transformations
 * 
 * @author ejoliet
 * @version $Id: ColorTransform.java 30580 2007-09-05 15:02:18Z nbach $
 */
public interface ColorTransform {
	/**
	 * Converts a value into a {@link Color} using the actual color
	 * tranformation, and minimum and maximum value.
	 * 
	 * @param val
	 *            value to convert into Color
	 * @param min
	 *            minimum value
	 * @param max
	 *            maximum value
	 * 
	 * @return a corresponding transformed color
	 */
	public Color getColor(double val, double min, double max);

	/**
	 * Compute the color transformation.
	 */
	public void computeTransform();

	/**
	 * Sets the color scale factor
	 * 
	 * @param f
	 *            color scale factor
	 */
	public void setScaleColor(double f);

	/**
	 * Gets the color scale factor
	 * 
	 * @return color scale factor
	 */
	public double getScaleColor();

	/**
	 * Gets the minimum data value used by this color transformation.
	 * 
	 * @return minimum data value used
	 */
	public double getMin();

	/**
	 * Gets the maximum data value used by this color transformation.
	 * 
	 * @return maximum data value used 
	 */
	public double getMax();
}