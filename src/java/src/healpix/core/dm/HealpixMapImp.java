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
import healpix.essentials.HealpixBase;
import healpix.essentials.Pointing;
import healpix.essentials.Scheme;
import healpix.core.dm.util.HealpixTool;
import healpix.tools.Constants;

import java.io.FileOutputStream;
import java.io.Serializable;

import nom.tam.fits.BinaryTableHDU;
import nom.tam.fits.Fits;
import nom.tam.fits.FitsFactory;
import nom.tam.util.BufferedDataOutputStream;

/**
 * A Healpix map can generate sky maps encoded in the HEALPix sky indexing
 * scheme (http://healpix.sourceforge.net/).
 *
 * The created maps can be written to a FITS data set for further processing
 * with e.g. visualisation/analysis tools in the HEALPix distribution.
 *
 * @author ejoliet
 * @version $Id: HealpixMapImp.java 140496 2010-06-23 12:52:12Z womullan $
 */
public class HealpixMapImp extends HealpixBase implements HealpixMap,
		Serializable, Cloneable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Each HEALPix cell will contain a MapItem. For now this is simply a
	 * wrapper around a value accumulator with an associated counter.
	 */
	private class MapItem implements Serializable {

		/**
		 *
		 */
		private static final long serialVersionUID = 1L;

		/** The val. */
		private double val = 0.; // value

		/** The n. */
		private int n = 0; // counter

		/**
		 * Add a value to the HEALPix cell
		 *
		 * @param val
		 *            Value to add
		 */
		public void add(double val) {
			this.val += val;
			++n;
		}

		/**
		 * Return the accumultade value of this pixel.
		 *
		 * @return the accumulated value of this HEALPix cell
		 */
		public double getValue() {
			return this.val;
		}

		/**
		 * Set the value into this map cell
		 *
		 * @param cellVal
		 *            the value to set
		 */
		public void setValue(double cellVal) {
			this.val = cellVal;
		}

		/**
		 * Return the counter indicating how many values have been accumulated
		 * in this cell.
		 *
		 * @return the counter indicating how many values have been accumulated
		 *         in this cell
		 */
		public int getN() {
			return this.n;
		}

		/**
		 * Average the values accumulated in the cell.
		 */
		public void average() {
			if (this.n != 0) {
				scale((double) 1.0 / this.n);
			}
		}

		/**
		 * Scale the cell value by the input value.
		 *
		 * @param f
		 *            Cell scaling value
		 */
		public void scale(double f) {
			this.val *= f;
		}
	}

	/* Number of maps. */
	/** The n maps. */
	private short nMaps = 0;

	/* Array of string names of the maps. */
	/** The map names. */
	private String[] mapNames = null;

	/* Map elements. */
	/** The map. */
	private MapItem[][] map = null;

	/* Number of elements accumulated in each map. */
	/** The N. */
	private int N[] = null;

	/* Minimum value accumulated in each map. */
	/** The min val. */
	private double[] minVal = null;

	/* Maximum value accumulated in each map. */
	/** The max val. */
	private double[] maxVal = null;

	/** The colname. */
	private String[] colname;

	/** The imap. */
	private int imap = 0;

	private final HealpixMapImp map_orig;

	private CoordSys coordSys;

	/**
	 * Construct a HEALPix mapper for a given map names and data columns.
	 *
	 * @param mapIt
	 *            the data array
	 * @param maps
	 *            Array of strings with map names. the length of the array
	 *            determines the number of maps to generate.
	 * @throws Exception
	 */
	public HealpixMapImp(String maps[], double[][] mapIt) throws Exception {
		super((int) Math.round(Math.sqrt(mapIt[0].length/12.)),Scheme.RING);

		this.mapNames = maps;
		setName(maps);
		this.nMaps = (short) this.mapNames.length;
		if (this.nMaps == 0) {
			throw new Exception("Cannot construct empty Healpix map");
		}
		this.map = new MapItem[this.nMaps][];
		this.minVal = new double[this.nMaps];
		this.maxVal = new double[this.nMaps];
		this.N = new int[this.nMaps];
		for (int n = 0; n < this.nMaps; ++n) {
			int M = (int)this.nPixel();
			this.map[n] = new MapItem[M];
			for (int m = 0; m < M; ++m) {
				this.map[n][m] = new MapItem();
				this.map[n][m].setValue(mapIt[n][m]);
			}
		}
		for (int n = 0; n < this.nMaps; ++n) {
			this.minVal[n] = getMinMapItem(n);
			this.maxVal[n] = getMaxMapItem(n);
			this.N[n] = 0;
		}
		map_orig = this;
	}

	/**
	 * Construct a HEALPix mapper for a given NSIDE.
	 *
	 * @param nsideIndex
	 *            HEALPix sphere tesselisation is done with NSIDE=2^nSideIndex,
	 *            nSideIndex must be greater or equal to 0.
	 * @param maps
	 *            Array of strings with map names. the length of the array
	 *            determines the number of maps to generate.
	 * @throws Exception
	 */
	public HealpixMapImp(short nsideIndex, String maps[]) throws Exception {
		super(1L<<nsideIndex,Scheme.RING);

		this.mapNames = maps;
		setName(maps);
		this.nMaps = (short) this.mapNames.length;
		if (this.nMaps == 0) {
			throw new Exception("Cannot construct empty Healpix map");
		}
		this.map = new MapItem[this.nMaps][];
		this.minVal = new double[this.nMaps];
		this.maxVal = new double[this.nMaps];
		this.N = new int[this.nMaps];
		for (int n = 0; n < this.nMaps; ++n) {
			int M = (int)this.nPixel();
			this.map[n] = new MapItem[M];
			for (int m = 0; m < M; ++m) {
				this.map[n][m] = new MapItem();
			}
			this.minVal[n] = Double.MAX_VALUE;
			this.maxVal[n] = -Double.MAX_VALUE;
			this.N[n] = 0;
		}
		map_orig = this;
	}

	/**
	 * Construct a HEALPix mapper whose sphere tesselisation contains no less
	 * than a given number of pixels.
	 *
	 * @param pixels
	 *            Minimum number of cells/pixels that the tesselisation shall
	 *            contain.
	 * @param maps
	 *            Names of Healpix maps to create.
	 * @throws Exception
	 */
	public HealpixMapImp(long pixels, String maps[]) throws Exception {
		// Number of pixels in Healpix sphere is 12*nside^2
		// the expression below ensures that sphere tesselisation
		// with at least <pixels> pixels is constructed
		this((short) Math.ceil(Math.log(Math.sqrt((double) pixels / 12.))
				/ Math.log(2.)), maps);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#nPixel()
	 */
	public long nPixel() {
		return super.getNpix();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#nside()
	 */
	public int nside() {
		return (short) this.nside;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#add(healpix.core.AngularPosition, double)
	 */
	public void add(AngularPosition pos, double val) {
		add(0, pos, val);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#get(healpix.core.AngularPosition)
	 */
	public double get(AngularPosition pos) {

		try {
			int index = this.ang2pix(pos);

			return map[0][index].getValue();
		} catch (Exception e) {

			// argument out of bounds - ignore this point
			System.out.println("Could not convert position " + pos.toString()
					+ " to HEALpix index " + " ignoring this item");

		}

		return 0.0;
	}

	/**
	 * Get the accumulated added value to a pixel
	 *
	 * @param i
	 *            ith map
	 * @param pixId
	 *            pixel id
	 * @return number of added value
	 */
	public int getCount(int i, int pixId) {
		return map[i][pixId].getN();
	}

	/**
	 * Get the accumulated added value to a pixel
	 *
	 * @param i
	 *            ith map
	 * @param pos
	 *            Angular position in sky
	 * @return number of added value
	 * @throws Exception
	 */
	public int getPosCount(int i, AngularPosition pos) throws Exception {
		int pixId = this.ang2pix(pos);
		return map[i][pixId].getN();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#get(int, int)
	 */
	public double get(int i, int pixId) {
		return map[i][pixId].getValue();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#get(int)
	 */
	public double get(int pixId) {
		return map[0][pixId].getValue();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#add(int, double)
	 */
	public void add(int index, double val) {
		MapItem item = this.map[0][index];
		if (item == null) {
			item = this.map[0][index] = new MapItem();

		}
		item.add(val);
		if (item.val > this.maxVal[0]) {
			this.maxVal[0] = item.val;
		}
		if (item.val < this.minVal[0]) {
			this.minVal[0] = item.val;
		}
		this.N[0]++;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#add(int, healpix.core.AngularPosition,
	 *      double)
	 */
	public void add(int i, AngularPosition pos, double val) {
		if (i < 0 || i > this.nMaps) {
			// ignore
			return;
		}

		try {
			int index = (int) (this.ang2pix(pos));
			MapItem item = this.map[i][index];
			if (item == null) {
				item = this.map[i][index] = new MapItem();

			}
			item.add(val);
			if (item.val > this.maxVal[i]) {
				this.maxVal[i] = item.val;
			}
			if (item.val < this.minVal[i]) {
				this.minVal[i] = item.val;
			}
			this.N[i]++;
		} catch (Exception e) {
			// argument out of bounds - ignore this point
			System.out.println("Could not convert postion " + pos.toString()
					+ " to HEALpix index " + " igoring this item");
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#normalize(int)
	 */
	public void normalize(int n) {
		for (MapItem i : this.map[n]) {
			i.scale(1.0 / (double) this.N[n]);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#average(int)
	 */
	public void average(int n) {
		for (MapItem i : this.map[n]) {
			i.average();
		}

		// re-calculate min/max
		this.maxVal[n] = this.minVal[n] = 0.;
		for (MapItem i : this.map[n]) {
			if (i.val > this.maxVal[n]) {
				this.maxVal[n] = i.val;
			}
			if (i.val < this.minVal[n]) {
				this.minVal[n] = i.val;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#scale(int, double)
	 */
	public void scale(int n, double f) {
		for (MapItem i : this.map[n]) {
			i.setValue(i.getValue() * f);
		}
		this.maxVal[n] *= f;
		this.minVal[n] *= f;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#toDataSet(java.lang.String)
	 */
	public void toDataSet(String name) throws Exception {
		double[][] mapData = new double[this.nMaps][];
		Object[] table = new Object[this.nMaps];
		for (int n = 0; n < this.nMaps; ++n) {
			mapData[n] = new double[(int)this.npix];
			table[n] = mapData[n];

			// populate column data
			for (int k = 0; k < this.npix; ++k) {
				MapItem i = this.map[n][k];

				mapData[n][k] = i != null ? i.getValue() : 0.;
			}
		}

		// Creating a binary Fits table
		FitsFactory.setUseAsciiTables(false);
		FitsFactory.setUseHierarch(true);
		Fits f = new Fits();
		f.addHDU(Fits.makeHDU(table));
		BinaryTableHDU bhdu = (BinaryTableHDU) f.getHDU(1);

		// Add the columns names
		for (int n = 0; n < this.nMaps; ++n) {
			bhdu.setColumnName(n, this.mapNames[n], "values");
		}

		// Add the values
		bhdu.addValue("PIXTYPE", "HEALPIX", "This is a HEALPix map");
		bhdu.addValue("NSIDE", this.nside(), "HEALPix NSIDE parameter");
		bhdu.addValue("ORDERING", this.scheme.toString().toUpperCase(),
				"HEALPix ordering scheme");
		bhdu.addValue("COORDSYS", getCoordSys().fitsType, "Coordinate system of map");

		for (int n = 0; n < this.nMaps; ++n) {
			bhdu.addValue("TDMIN" + (n + 1), this.minVal[n],
					"minimum actual value");
			bhdu.addValue("TDMAX" + (n + 1), this.maxVal[n],
					"maximum actual value");
		}

		FileOutputStream fos = new FileOutputStream(name);
		BufferedDataOutputStream s = new BufferedDataOutputStream(fos);

		f.write(s);
		s.flush();
		s.close();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#regrade(int)
	 */
	public HealpixMap regrade(int nside) {
		HealpixMap m = null;

		if (nside > this.nside()) { // Upgrade!
			try {
				m = new HealpixTool(map_orig).upgrade(nside);
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else if (nside < this.nside()) { // Degrade!
			try {
				m = new HealpixTool(map_orig).degrade(nside);
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			m = map_orig;
		}
		// At least return the same map!
		return m;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#getMin()
	 */
	public double getMin() {
		return (double) this.minVal[0];
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#getMax()
	 */
	public double getMax() {
		return (double) this.maxVal[0];
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#getMax(int)
	 */
	public double getMax(int i) {
		return this.maxVal[i];
//		double max = Double.MIN_VALUE;
//		double min = Double.MAX_VALUE;
//		long npix = 12 * nside * nside;
//		for (int u = 0; u < npix; u++) {
//			// double val = rch.getPixel(i);
//			double val = (double) get(i, u);
//			if (val < min)
//				min = val;
//			if (val > max)
//				max = val;
//		}
//		return (double) max;
	}

	/**
	 * Gets the max map item.
	 *
	 * @param i the i
	 *
	 * @return the max map item
	 */
	public double getMaxMapItem(int i) {
		double max = -Double.MAX_VALUE;
		double min = Double.MAX_VALUE;
		long npix = 12 * nside * nside;
		for (int u = 0; u < npix; u++) {
			// double val = rch.getPixel(i);
			double val = this.map[i][u].getValue();
			if (val < min)
				min = val;
			if (val > max)
				max = val;
		}
		return max;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#getMin(int)
	 */
	public double getMin(int i) {
		return this.minVal[i];
//		double max = Double.MIN_VALUE;
//		double min = Double.MAX_VALUE;
//		long npix = 12 * nside * nside;
//		for (int u = 0; u < npix; u++) {
//			// double val = rch.getPixel(i);
//			double val = (double) get(i, u);
//			if (val < min)
//				min = val;
//			if (val > max)
//				max = val;
//		}
//		return (double) min;
	}

	/**
	 * Gets the min map item.
	 *
	 * @param i the i
	 *
	 * @return the min map item
	 */
	public double getMinMapItem(int i) {
		double max = -Double.MAX_VALUE;
		double min = Double.MAX_VALUE;
		long npix = 12 * nside * nside;
		for (int u = 0; u < npix; u++) {
			double val = this.map[i][u].getValue();
			if (val < min)
				min = val;
			if (val > max)
				max = val;
		}
		return min;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#setValueCell(int, int, double)
	 */
	public void setValueCell(int nmap, int ipix, double val) {
		MapItem item = map[nmap][ipix];
		if (item == null) {
			item = map[nmap][ipix] = new MapItem();
		}
		item.setValue(val);
		if (item.val > this.maxVal[nmap]) {
			this.maxVal[nmap] = item.val;
		}
		if (item.val < this.minVal[nmap]) {
			this.minVal[nmap] = item.val;
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#setValueCell(int, double)
	 */
	public void setValueCell(int ipix, double val) {
		setValueCell(0, ipix, val);
	}

	/**
	 * Return the value of the HEALPix NSIDE parameter.
	 *
	 * @return the value of HEALPIx NSIDE parameter.
	 */
	public int getNside() {
		return nside();
	}

	/**
	 * Setting the resolution number nside
	 *
	 * @param nside
	 *            resolution number
	 */
	public void setNside(int nside) {
		this.nside = nside;

	}


	public int ang2pix(double theta, double phi) throws Exception {
		return (int) super.ang2pix(new Pointing(theta,phi));
	}

	/**
	 * Get the map ith pixel from an {@link AngularPosition} position
	 *
	 * @param pos
	 *            the {@link AngularPosition} position
	 * @return healpix ith pixel number
	 * @throws Exception
	 */
	public int ang2pix(AngularPosition pos) throws Exception {
		return (int) super.ang2pix(pos);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#pix2ang(long)
	 */
	public AngularPosition pix2ang(long ipix) throws Exception {
		return new AngularPosition(super.pix2ang(ipix));
	}

	/**
	 * Method to get the projected map dependent of the scheme : will obtain
	 * xsize as x-dimension of the output-array and lon0 as the longitude (in
	 * degrees)
	 */
	public double[] mollpro(int xsize, double lon0) {
		int ysize = (int) (xsize / 2);
		int xc = (int) ((xsize - 1) / 2);
		int dx = xc;
		int yc = (int) ((ysize - 1) / 2);
		int dy = yc;
		double[][] u = new double[xsize][ysize];
		double[][] v = new double[xsize][ysize];
		double[][] out_lon = new double[xsize][ysize];
		double[][] out_lat = new double[xsize][ysize];
		double[][] mask = new double[xsize][ysize];
		double[] propixels = new double[xsize * ysize];
		double DtoR = Constants.PI / ((double) 180);
		double pih = Constants.piover2;
		double lon0rad = lon0 * DtoR;

		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				u[i][j] = (double) i;
				v[i][j] = (double) j;
			}
		}
		// Construction of the ellipse points
		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				u[i][j] = 2.0 * (u[i][j] - (double) xc) / ((double) dx);// 1.02);//Math.sqrt(2)
				v[i][j] = (v[i][j] - (double) yc) / ((double) dy);// 1.02);//Math.sqrt(2)
			}
		}
		// Method to select only the poitns inside the ellipse.
		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				mask[i][j] = u[i][j] * u[i][j] * 0.25 + v[i][j] * v[i][j];
			}
		}

		// for each point on the mollweide map looks for the corresponding
		// position (lon, lat) on the sphere
		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				// values outside the ellipse
				if (mask[i][j] > 1.0) {
					out_lat[i][j] = Math.pow(10.0, 12.0);
					out_lon[i][j] = Math.pow(10.0, 12.0);
				} else {
					double e = .000001f;
					// Latitude(phi) comes from 2*theta + sin(2*theta) =
					// pi*sin(phi)
					// Wiliam's code... i think this the bug, it makes the map
					// flip over x axis
					// corrected...
					out_lat[i][j] = (Math.asin(2.0
							/ Constants.PI
							* (Math.asin(v[i][j]) + v[i][j]
									* Math.sqrt((1.0 - v[i][j])
											* (1.0 + v[i][j])))));
					// The old code to keep!:
					out_lat[i][j] = pih - out_lat[i][j];
					// colat in [0,pi]
					// Wiliam's code...
					// i think this the bug, it makes the map
					// flip over y axis
					out_lon[i][j] = -lon0rad
							- 0.5
							* Constants.PI
							* u[i][j]
							/ (Math.max(Math.sqrt((1.0 - v[i][j])
									* (1.0 + v[i][j])), e));
					// lon in [-pi,pi], the minus sign for astro convention
					// corrected...
					out_lon[i][j] = -out_lon[i][j];
				}
				if (out_lon[i][j] < 0.0)
					out_lon[i][j] = out_lon[i][j] + 2.0 * Constants.PI;
				// lon in RAD in [0,2pi]
			}
		}

		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				propixels[j + ysize * i] = -1.0;
				// white background
			}
		}

		// converts the position on the sphere into pixel number and project the
		// corresponding data value on the map

		for (int i = 0; i < xsize; i++) {
			for (int j = 0; j < ysize; j++) {
				if (Math.abs(out_lat[i][j]) < 100000.0) {
					int ipix = 0;
					try {
						// AngularPosition ang = new
						// AngularPosition(out_lon[i][j], out_lat[i][j]);
						ipix = this.ang2pix(out_lat[i][j], out_lon[i][j]);
						propixels[i + xsize * j] = (double) this
								.get(imap, ipix);
					} catch (Exception e) {
						e.printStackTrace();
						System.out.println("ipix " + ipix);
					}
				}
			}
		}
		return propixels;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#getName()
	 */
	public String[] getName() {
		return colname;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#setName(java.lang.String[])
	 */
	public void setName(String[] colname) {
		this.colname = colname;

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#getImap(java.lang.String)
	 */
	public int getImap(String cname) {
		for (int t = 0; t < this.getName().length; t++) {
			if (cname.equals(this.getName()[t])) {
				this.imap = t;
			}
		}
		return imap;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.AbstractHealpixMap#setImap(int)
	 */
	public void setImap(int i) {
		this.imap = i;
	}

	/**
	 * makes the conversion map NEST to RING
	 *
	 * @throws Exception
	 */
	public void convert_nest2ring() throws Exception {
		int ipn = 0;
		setScheme(Scheme.RING);
		for (int imap = 0; imap < this.getName().length; imap++) {
			for (int ipr = 0; ipr < nPixel(); ipr++) {
				ipn = (int)this.nest2ring(ipr);
				MapItem item = map[imap][ipr];
				double val = item.getValue();
				if (item == null) {
					item = map[imap][ipn] = new MapItem();
				}
				item.setValue(val);
			}
		}
	}

	/**
	 * makes the conversion map RING to NEST
	 *
	 * @throws Exception
	 */
	public void convert_ring2nest() throws Exception {
		int ipn = 0;
		setScheme(Scheme.NESTED);
		for (int imap = 0; imap < this.getName().length; imap++) {
			for (int ipr = 0; ipr < nPixel(); ipr++) {
				ipn = (int) this.ring2nest(ipr);
				MapItem item = map[imap][ipr];
				double val = item.getValue();
				if (item == null) {
					item = map[imap][ipn] = new MapItem();
				}
				item.setValue(val);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#getMapItemData()
	 */
	public double[][] getMapItemData() {
		int npix = (int)(12 * nside * nside);
		double[][] mapDouble = new double[this.nMaps][npix];
		for (int j = 0; j < this.nMaps; j++) {
			for (int u = 0; u < npix; u++) {
				mapDouble[j][u] = get(j, u);
			}
		}
		return mapDouble;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see healpix.core.dm.HealpixMap#mean(int, int, int)
	 */
	public double mean(int imap, int firstPix, int lastPix) {
		int total = lastPix - firstPix + 1;
		double sum = 0.0d;
		for (int ipr = firstPix; ipr < lastPix + 1; ipr++) {
			// System.out.println("ipr="+ipr);
			MapItem item = map[imap][ipr];
			double val = item.getValue();
			sum += val;
		}
		return sum / total;
	}

	/**
	 * Shallow clone.
	 *
	 * @return the object
	 */
	public Object shallowClone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			throw (Error) new InternalError().initCause(e); // impossible
		}
	}

	@Override
	public CoordSys getCoordSys() {
		if(coordSys==null){
			coordSys = CoordSys.fromFits('G');
		}
		return coordSys;
	}

	@Override
	public void setCoordSys(CoordSys cs) {
		coordSys=cs;
	}

	/**
	 * @see healpix.core.dm.HealpixMap#getUnit(short)
	 */
	@Override
	public String getUnit(short mapIndex) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see healpix.core.dm.HealpixMap#setUnit(java.lang.String, short)
	 */
	@Override
	public void setUnit(String unit, short mapIndex) {
		// TODO Auto-generated method stub

	}
}
