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

import healpix.core.AngularPosition;

/**
 * Healpix map data model interface.
 * 
 * @version $Id: HealpixMap.java 140496 2010-06-23 12:52:12Z womullan $
 * @author ejoliet
 * 
 */
public interface HealpixMap extends AbstractHealpixMap {

	/**
	 * Returns the index healpix pixel from an angular position
	 * 
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return integer index pixel
	 * @throws Exception
	 */
	public int ang2pix(double theta, double phi) throws Exception;

	/**
	 * Returns the angular position {@link AngularPosition} pointing at that
	 * pixel index in the healpix sphere tesslated
	 * 
	 * @param ipix
	 *            the index pixel to get the angular position
	 * @return the angular position theta,phi
	 * @throws Exception
	 */
	public AngularPosition pix2ang(long ipix) throws Exception;

	/**
	 * Add a value to a HEALPix cell in the default map.
	 * 
	 * @param pos
	 *            Position of HEALPix cell whose value is to be incremented -
	 *            spherical coordinates (0<=theta<=Pi; 0<=phi<=2Pi).
	 * @param val
	 *            Value to add to the HEALPix cell.
	 */
	public void add(AngularPosition pos, double val);

	/**
	 * Return the value of an HEALPix cell in the default map.
	 * 
	 * @param pos
	 *            Position of HEALPix cell whose value is to be incremented -
	 *            spherical coordinates (0<=theta<=Pi; 0<=phi<=2Pi).
	 * @return the value of the cell.
	 */
	public double get(AngularPosition pos);

	/**
	 * Return the value of an HEALPix cell in the map.
	 * 
	 * @param imap
	 *            Map index in the range 0 to number of maps-1
	 * @param pixId
	 *            Pixel id.
	 * @return the value of the cell.
	 */
	public double get(int imap, int pixId);

	/**
	 * Return the value of an HEALPix cell in the default map.
	 * 
	 * @param pixId
	 *            Pixel id.
	 * @return the value of the cell.
	 */
	public double get(int pixId);

	/**
	 * Add a value to a HEALPix cell in the default map.
	 * 
	 * @param index
	 *            Pixel Id.
	 * @param val
	 *            Value to add to the HEALPix cell.
	 */
	public void add(int index, double val);

	/**
	 * Add a value to a HEALPix cell in a map
	 * 
	 * @param i
	 *            Map index in the range 0 to number of maps-1
	 * @param pos
	 *            Position of HEALPix cell whose value is to be incremented -
	 *            spherical coordinates (0<=theta<=Pi; 0<=phi<=2Pi).
	 * @param val
	 *            Value to add to the HEALPix cell.
	 */
	public void add(int i, AngularPosition pos, double val);

	/**
	 * Scale a map by total number of values added, i.e. the sum of all cell
	 * values amounts to one after the operation
	 * 
	 * @param n
	 *            Index of map to normalise.
	 */
	public void normalize(int n);

	/**
	 * Replace each cell value by the average of all values added to the cell.
	 * 
	 * @param n
	 *            Index of map.
	 */
	public void average(int n);

	/**
	 * Scale each cell value by the given value.
	 * 
	 * @param n
	 *            Index of map
	 * @param f
	 *            Value to multiply by.
	 */
	public void scale(int n, double f);

	/**
	 * Write generated maps to a FITS data set.
	 * 
	 * @param name
	 *            Name of the data set.
	 * @throws Exception
	 */
	public void toDataSet(String name) throws Exception;

	/**
	 * Returns minimum value from default map
	 * 
	 * @return the minimum value from ith map
	 */
	public double getMin();

	/**
	 * Returns maximum value from default map.
	 * 
	 * @return the maximum value from default map
	 */
	public abstract double getMax();

	/**
	 * Returns maximum value from a map.
	 * 
	 * @param i
	 *            a map index from 0 to nMap-1
	 * @return the maximum value from ith map
	 */
	public double getMax(int i);

	/**
	 * Returns minimum value from a map.
	 * 
	 * @param i
	 *            a map index from 0 to nMap-1
	 * @return the minimum value from ith map
	 */
	public double getMin(int i);

	/**
	 * Sets the value val into the cell numbered ipix in nmap map
	 * 
	 * @param nmap
	 *            the nth map
	 * @param ipix
	 *            the ith pixel
	 * @param val
	 *            the double value to set
	 */
	public void setValueCell(int nmap, int ipix, double val);

	/**
	 * Sets the value val into the cell numbered ipix in default map
	 * 
	 * @param ipix
	 *            the ith pixel
	 * @param val
	 *            the double value to set
	 */
	public void setValueCell(int ipix, double val);

	/**
	 * Method to get the projected map dependent of the scheme : will obtain
	 * xsize as x-dimension of the output-array and lon0 as the longitude (in
	 * degrees)
	 */
	public double[] mollpro(int xsize, double lon0);

	/**
	 * Down/Upgrade the map resolution
	 * 
	 * @param nside
	 *            resolution number
	 * @return {@link HealpixMap}
	 */
	public HealpixMap regrade(int nside);

	/**
	 * makes the conversion map RING to NEST
	 * 
	 * @throws Exception
	 */
	public void convert_ring2nest() throws Exception;

	/**
	 * Makes the conversion map NEST to RING
	 * 
	 * @throws Exception
	 */
	public void convert_nest2ring() throws Exception;

	/**
	 * Average value over data from firstPix to lastPix index
	 * 
	 * @param nmap
	 *            the ith map
	 * @param firstPix
	 *            first pixel index to count
	 * @param lastPix
	 *            last pixel index to count
	 * @return the arithmetic mean
	 */
	public double mean(int nmap, int firstPix, int lastPix);

	/**
	 * Gets the column(s) data values
	 * 
	 * @return the map(s) double values
	 */
	public double[][] getMapItemData();
	
	/**
	 * Coordinate system for this map - will put C,E,G in fits file.
	 */
	public enum CoordSys {
	/** C - Equatorial for some of us*/
	CELESTIAL('C'),
	/** E */
	ECLIPTIC('E'), 
	/** G  */
	GALACTIC('G');
	
	/** the FitsType char to go in COORDSYS Keyword*/
	public char fitsType;
	
	/**
	 * 
	 * @param c
	 * @return CoordSys from the given CHAR
	 */
	public static CoordSys fromFits(char c) {
		switch (c) {
		case 'G' : return GALACTIC ; 
		case 'E' : return ECLIPTIC; 
		case 'C' : return CELESTIAL;
		}
		return CELESTIAL;
	}
		
	CoordSys(char ft) {
			this.fitsType=ft;
	    }
	} ;
	
	/**
	 * set the Coordsys.
	 * Really just for the fits file
	 * @param cs
	 */
	public void setCoordSys(CoordSys cs);
	
	/**
	 * return current coordsys
	 * @return which coordSys is set
	 */
	public CoordSys getCoordSys();
	
	/**
	 * Get the unit of the map of index mapIndex
	 * @param mapIndex
	 * @return
	 */
	public String getUnit(short mapIndex);
	
	/**
	 * Sets unit of the map index mapIndex
	 * @param unit
	 * @param mapIndex
	 */
	public void setUnit(String unit, short mapIndex);
	
}