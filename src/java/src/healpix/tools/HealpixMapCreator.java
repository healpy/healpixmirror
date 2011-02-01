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

import healpix.core.dm.HealpixMap;
import healpix.core.dm.HealpixMapImp;
import healpix.fits.Fits2HealpixMapImp;

import java.io.File;

/**
 * A HealpixMap creator. This is a convenience factory to get an empty map or to
 * load a map from a fits file.
 * 
 * @author ejoliet
 * @version $Id: HealpixMapCreator.java 56224 2008-07-30 07:30:00Z ejoliet $
 */
public class HealpixMapCreator {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The colname. */
	public String[] colname;

	/** The imap. */
	int imap = 0;

	/** The map. */
	HealpixMap map;

	/**
	 * Instantiates a new healpix map creator.
	 */
	public HealpixMapCreator() {
		try {
			generateMap(1);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Creates a {@link HealpixMap} from a fits file.
	 * 
	 * @param fitsname
	 *            Fits file name
	 */
	public HealpixMapCreator(String fitsname) {
		try {
			if (new File(fitsname).exists())
				generateMapFromFits(fitsname);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Generate the {@link HealpixMap} from a fits.
	 * 
	 * @param filename
	 * @throws Exception
	 */
	private void generateMapFromFits(String filename) throws Exception {
		Fits2HealpixMapImp fmap = new Fits2HealpixMapImp();
		HealpixMap map = fmap.fits2map(filename);
		fmap.printTable();
		// colname = fmap.names;
		setMap(map);
		// generateMoll();

	}

	/**
	 * Creates a {@link HealpixMap} from map names and values.
	 * 
	 * @param mapNames
	 *            String map names
	 * @param mapDouble
	 *            columns data values
	 */
	public HealpixMapCreator(String[] mapNames, double[][] mapDouble) {

		try {
			HealpixMap map = new HealpixMapImp(mapNames, mapDouble);
			setMap(map);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	/**
	 * Create an HEALPix map with given depth {@link HealpixMap}.
	 * 
	 * @param nside -
	 *            the nside of the map required
	 */
	public HealpixMapCreator(int nside) {
		int depth = (int) (Math.log(nside) / Math.log(2));
		if (Constants.verbose > 0) {
			System.out.println("*** Order/depthStatic creation=" + depth);
		}
		try {
			generateMap(depth);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Instantiates a new healpix map creator.
	 * 
	 * @param nside the nside
	 * @param isEmpty the is empty
	 */
	public HealpixMapCreator(int nside, boolean isEmpty) {
		int depth = (int) (Math.log(nside) / Math.log(2));
		if (Constants.verbose > 0) {
			System.out.println("*** Order/depthStatic creation=" + depth);
		}
		
		try {
			if(!isEmpty) {
				generateMap(depth);
			}else {
				generateEmptyMap(depth);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * @param depth
	 * @throws Exception 
	 */
	private void generateEmptyMap(int depth) throws Exception {
		HealpixMapImp map = new HealpixMapImp((short) depth,
				new String[] { "DATA" });
		System.out.println("Name[0]:" + map.getName()[0]);
		System.out.println("Nside:" + map.getNside());
		System.out.println("Npixels:" + map.nPixel());
		setMap(map);
		
	}

	/**
	 * Generate a simple map, setting the value to 1 at ipix = 0
	 * 
	 * @param nside
	 * @throws Exception
	 */
	private void generateMap(int depth) throws Exception {
		HealpixMapImp map = new HealpixMapImp((short) depth,
				new String[] { "DATA1", "DATA2" });
		System.out.println("Name[0]:" + map.getName()[0]);
		System.out.println("Nside:" + map.getNside());
		System.out.println("Npixels:" + map.nPixel());
		map.setValueCell(0, 0, 1);
		map.setValueCell(1, 1, 2);
		setMap(map);
	}

	/**
	 * Gets the map.
	 * 
	 * @return the map
	 */
	public HealpixMap getMap() {
		return map;
	}

	/**
	 * Sets the map.
	 * 
	 * @param m the new map
	 */
	public void setMap(HealpixMap m) {
		map = m;
	}
}
