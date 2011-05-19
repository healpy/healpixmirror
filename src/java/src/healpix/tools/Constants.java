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
 * The Class Constants.
 */
public class Constants {
	
	/** Magic number from Martin Reinecke for fudging search radius**/
	public static final double magic = 1.362;

	/** The Constant PI. */
	public static final double PI = Math.PI;//3.141592653589793238462643383279502884197;

	/** The Constant cPr. */
	public static final double cPr = PI / 180;

	/** The Constant vlev. */
	public static final int vlev = 2;

	/** The Constant EPS. */
	public static final double EPS = 0.0000001;

	/** The Constant c. */
	public static final double c = 0.105;

	/** The verbose. */
	public static int verbose = Integer.parseInt(System.getProperty("verbose",
			"1"));

	/** The Constant ln10. */
	public static final double ln10 = Math.log(10);

	/** The Constant piover2. */
	public static final double piover2 = PI / 2.;

	/** The Constant twopi. */
	public static final double twopi = 2*PI;//6.283185307179586476925286766559005768394;// 2 *
																					// PI;

	/** The Constant twothird. */
	public static final double twothird = 2. / 3.;

	/** The Constant 1 arcsecond in units of radians. */
	public static final double ARCSECOND_RADIAN = 4.84813681109536e-6;
}
