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
package healpix.plot3d.gui.healpix3d;

import healpix.essentials.Pointing;
import healpix.essentials.HealpixBase;

/**
 * This class is used to construct the quad geometry for the 3d sphere.
 *
 * @see QuadArrayExt
 * @author ejoliet
 * @version $Id: HealpixDataIndex.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class HealpixDataIndex extends HealpixBase {
	/**
	 * Default serial version
	 */
	private static final long serialVersionUID = 1L;

	/** The angle. */
	Pointing angle;

	/** The ipix. */
	int ipix;

	/** The value. */
	double value;

	/**
	 * Default constructor
	 */
	public HealpixDataIndex() {
		ipix = 0;
		value = 0.0;
		angle = new Pointing();
	}

}
