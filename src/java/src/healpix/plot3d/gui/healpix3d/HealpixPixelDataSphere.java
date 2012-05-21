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

import healpix.core.dm.HealpixMap;
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
 * @version $Id: HealpixPixelDataSphere.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class HealpixPixelDataSphere extends HealSphere {

	/** The Constant BASE_BLUE_FACTOR. */
	private static final double BASE_BLUE_FACTOR = 0.19f;

	/** The Constant BASE_GREEN_FACTOR. */
	private static final double BASE_GREEN_FACTOR = 0.29f;

	/** The Constant BASE_RED_FACTOR. */
	private static final double BASE_RED_FACTOR = 0.36f;

	/** The Constant BASE_BLUE_OFFSET. */
	private static final double BASE_BLUE_OFFSET = 0.029f;

	/** The Constant BASE_GREEN_OFFSET. */
	private static final double BASE_GREEN_OFFSET = -0.18f;

	/** The Constant BASE_RED_OFFSET. */
	private static final double BASE_RED_OFFSET = -0.46f;

	/** The face. */
	protected int face; // = 0, default initialization.

	/** The imap. */
	protected int imap;

	/** The q. */
	int q;

	/** The ch. */
	protected HealpixMap ch;

	/** The max. */
	protected double min, max;

	/**
	 * Used to get the data sphere from a ith map
	 *
	 * @param ch
	 *            the map data
	 * @param imap
	 *            the column data from this map ch
	 * @param iQuads
	 *            ith quad array
	 */
	public HealpixPixelDataSphere(HealpixMap ch, int imap, int iQuads) {
		super(ch.nside(),ch.getScheme());
		this.imap = imap;
		this.ch = ch;
		this.q = iQuads;
		// System.out.println("Map col name"+ch.getName()[imap]);
		this.min = ch.getMin(imap);
		this.max = ch.getMax(imap);
		this.setGeometry(createGeometry());
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
	 */
	protected Geometry createGeometry() {
		int ppq = 4; // points per quad
		// QuadArrayExt[] quads2 = new QuadArrayExt[nQuads];
		QuadArrayExt quads = new QuadArrayExt(ppq * 12
				* (int) Math.pow(nside, 2), GeometryArray.COORDINATES
				| GeometryArray.COLOR_3);

		// Specific colour scaling factors and offsets.
		double scaleColor = (double) Math.abs(max - min);
		double bc = BASE_BLUE_FACTOR * scaleColor;
		double gc = BASE_GREEN_FACTOR * scaleColor;
		double rc = BASE_RED_FACTOR * scaleColor;
		double bof = BASE_BLUE_OFFSET * scaleColor;
		double gof = BASE_GREEN_OFFSET * scaleColor;
		double rof = BASE_RED_OFFSET * scaleColor;

		int offset;
		Color3f c;

		try {
			// quads2[q] = new QuadArrayExt(4,GeometryArray.COORDINATES
			// | GeometryArray.COLOR_3);
			int pixindex = q;
			Vec3[] points = index.boundaries(pixindex, 1);
			double val = (double) ch.get(imap, pixindex);// ch.getPixAsFloat(pixindex);
			// System.out.println("********** val(ipix=" +
			// pixindex+"):"+val);
			if (Double.isNaN(val)) {
				c = new Color3f(128, 128, 128);
				// System.out.println("Val isNaN, color:"+c.toString());
			} else {
				// double blue = (double) Math.sin((val - min + bof) / bc);
				// double green = (double) Math.sin((val - min + gof) / gc);
				// double red = (double) Math.sin((val - min + rof) / rc);
				// IndexedColorMap cmap = new IndexedColorMap();
				float blue = (float) Math.sin((val - min + bof) / bc);
				float green = (float) Math.sin((val - min + gof) / gc);
				float red = (float) Math.sin((val - min + rof) / rc);
				if (red < 0)
					red = 0;
				if (blue < 0)
					blue = 0;
				if (green < 0)
					green = 0;
				c = new Color3f(red, green, blue);
				// System.out.println(q+".-val="+val+"/min="+min+":color::"+c.toString());
			}
			offset = q * ppq;
			// System.out.println("Points length:"+points.length);
			for (int v = 0; v < points.length; v++) {
				Point3d p3d = new Point3d(points[v].x, points[v].y,
						points[v].z);
				System.out.println(q + ".- point=" + v + "offset=" + offset);
				System.out.println("point setCoord(offset+v,...)="
						+ (offset + v));
				System.out.println("Points[v]=" + points[v].toString());
				quads.setCoordinate(offset + v, p3d);
				quads.setColor(offset + v, c);
			}
			quads.setAngle(ch.pix2ang(pixindex));
			quads.setIpix(pixindex);
			quads.setValue(ch.get(imap, pixindex));
			// }
			// System.out.println("********** End ipix=" + (faceoff + q - 1));
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(0);
		}

		return quads;
	}
}
