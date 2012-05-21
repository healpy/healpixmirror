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
package healpix.core.dm;

import healpix.essentials.Scheme;
import java.io.Serializable;

/**
 * This represent a generalized multi data map.
 * @author ejoliet
 * @version $Id: AbstractHealpixMap.java 135547 2010-05-13 13:40:26Z womullan $
 */
public interface AbstractHealpixMap extends Serializable {

	/**
	 * Return the value of the HEALPix NSIDE parameter.
	 *
	 * @return the value of HEALPIx NSIDE parameter.
	 */
	public int nside();

	/**
	 * Return the current HEALPix scheme.
	 *
	 * @return the current HEALPix scheme.
	 */
	public Scheme getScheme();

	/**
	 * Set the HEALPix scheme.
	 *
	 * @param scheme
	 *            Scheme to set.
	 */
	public void setScheme(Scheme scheme) throws Exception;

	/**
	 * Return the number of pixels/cells of the sphere tesselisation.
	 *
	 * @return the number of pixels/cells of the sphere tesselisation.
	 */
	public long nPixel();

	/**
	 * Set the names of the maps
	 *
	 * @param colname
	 *            names maps
	 */
	public void setName(String[] colname);

	/**
	 * Get the names from the {@link HealpixMap}
	 *
	 * @return String[] names
	 */
	public String[] getName();

	/**
	 * Get the number corresponding to that name cname
	 *
	 * @param mapName
	 *            map name which we want to get the ith number
	 * @return ith map
	 */
	public int getImap(String mapName);

	/**
	 * Sets the map data from its number in healpix map
	 *
	 * @param imap
	 *            map ith number
	 */
	public void setImap(int imap);
}
