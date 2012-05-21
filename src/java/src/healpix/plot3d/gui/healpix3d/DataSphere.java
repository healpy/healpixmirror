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

import healpix.essentials.HealpixBase;
import healpix.core.dm.HealpixMap;
import healpix.plot3d.canvas3d.SineColorTransform;
import healpix.essentials.Vec3;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;

/**
 * Despite name represents a single Healpix face. Uses a coloured quadrilateral
 * to indicate a data value for each pixel. DataSphere deals with different map
 * inside a HealpixMap object - e.g. read from fits file-.
 *
 * @version $Id: DataSphere.java 120812 2010-01-24 23:12:12Z ejoliet $
 */
public class DataSphere extends HealSphere {

	/** The face. */
	protected int face; // = 0, default initialization.

	/** The imap. */
	protected int imap = 0;

	/** The ch. */
	protected HealpixMap ch;

	/** The max. */
	protected double min, max;

	/** The trans. */
	SineColorTransform trans;

	/**
	 * Most detailed constructor.
	 *
	 * @param nside
	 *            the healpix resolution parameter.
	 * @param face
	 *            the face which this object should display.
	 * @param ch
	 *            the source of the data values.
	 * @param min
	 *            minimum value against which color should be scaled.
	 * @param max
	 *            maximum value against which color should be scaled.
	 */
	public DataSphere(int nside, int face, HealpixMap ch, double min, double max) {
		super(nside,ch.getScheme());
		this.face = face;
		this.ch = ch;
		this.min = min;
		this.max = max;
		trans = new SineColorTransform(this.min, this.max);
		this.setGeometry(createGeometry());
	}

	/**
	 * Instantiates a new data sphere.
	 *
	 * @param face the face
	 * @param ch the ch
	 */
	public DataSphere(int face, HealpixMap ch) {
		super(ch.nside(),ch.getScheme());
		System.out.println("********** Face#=" + face);
		this.face = face;
		this.ch = ch;
		this.min = ch.getMin(imap);
		this.max = ch.getMax(imap);
		trans = new SineColorTransform(this.min, this.max);
		this.setGeometry(createGeometry());
	}

	/**
	 * Used to get the data sphere from a ith map
	 *
	 * @param face
	 * @param ch
	 * @param imap
	 */
	public DataSphere(int face, HealpixMap ch, int imap) {
		super(ch.nside(),ch.getScheme());
		this.imap = imap;
		this.face = face;
		this.ch = ch;
		this.min = ch.getMin(imap);
		this.max = ch.getMax(imap);
		trans = new SineColorTransform(this.min, this.max);
		this.setGeometry(createGeometry());
	}

	/**
	 * Instantiates a new data sphere.
	 *
	 * @param face the face
	 * @param ch the ch
	 * @param imap the imap
	 * @param min the min
	 * @param max the max
	 */
	public DataSphere(int face, HealpixMap ch, int imap, double min, double max) {
		super(ch.nside(),ch.getScheme());
		this.imap = imap;
		this.face = face;
		this.ch = ch;
		this.min = (double) min;
		this.max = (double) max;
		trans = new SineColorTransform(this.min, this.max);
		this.setGeometry(createGeometry());
	}

	/**
	 * Instantiates a new data sphere.
	 *
	 * @param face the face
	 * @param ch the ch
	 * @param imap the imap
	 * @param min the min
	 * @param max the max
	 * @param index the index
	 */
	public DataSphere(int face, HealpixMap ch, int imap, double min, double max,
			HealpixBase index) {
		super(index);
		this.imap = imap;
		this.face = face;
		this.ch = ch;
		this.min = min;
		this.max = max;
		trans = new SineColorTransform(this.min, this.max);
		this.setGeometry(createGeometry());
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
	 */
	protected Geometry createGeometry() {
		int nQuads = (int) Math.pow(nside, 2); // one face
		int ppq = 4; // points per quad
		int nPoints = nQuads * ppq;
		int faceoff = nQuads * face;
		// QuadArrayExt[] quads2 = new QuadArrayExt[nQuads];
		QuadArrayExt quads = new QuadArrayExt(nPoints,
				GeometryArray.COORDINATES | GeometryArray.COLOR_3 );

		int offset;
		Color3f c;

		try {
			int q = 0;
			for (q = 0; q < nQuads; q++) {
				// quads2[q] = new QuadArrayExt(4,GeometryArray.COORDINATES
				// | GeometryArray.COLOR_3);
				int pixindex = faceoff + q;
				Vec3[] points = index.boundaries(pixindex, 1);
				double val = (double) ch.get(imap, pixindex);// ch.getPixAsFloat(pixindex);
//				if(val==0)
//					continue;
				if (Double.isNaN(val)) {
					c = new Color3f(128, 128, 128);
				} else {
					c = trans.getColor3f(val);
				}
				offset = q * ppq;

				for (int v = 0; v < points.length; v++) {
					quads.setCoordinate(offset + v, new Point3d(points[v].x,
							points[v].y, points[v].z));
					quads.setColor(offset + v, c);
					quads.setAngle(offset + v, ch.pix2ang(pixindex));
					quads.setIpix(offset + v, pixindex);
					quads.setValue(offset + v, ch.get(imap, pixindex));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return quads;
	}

	/**
	 * Gets the face num.
	 *
	 * @return the face num
	 */
	public int getFaceNum() {
		return this.face;
	}
}
