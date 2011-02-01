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
package healpix.plot3d.canvas3d;

import javax.media.j3d.Appearance;
import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.IndexedLineArray;
import javax.media.j3d.Shape3D;
import javax.vecmath.Point3d;

/**
 * A sphere shape, extending Shape3D, but drawn using longitude/latitude lines.
 * The radius of the sphere may also be given.
 * 
 * @author ejoliet
 * @version $Id: Group3DSphere.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class Group3DSphere extends Shape3D {
	
	/** The long res. */
	private int longRes = 12;

	/** The lat res. */
	private int latRes = 24;

	/** The radius. */
	private double radius = 1.0;

	/**
	 * Construct default sphere: longitudinal resolution 12, latitude 24, radius
	 * 1
	 */
	public Group3DSphere() {
		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Alternate constructor: specify radius
	 */
	public Group3DSphere(int rad) {
		radius = rad;

		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Alternate constructor: specify longitudinal and latitude resolution
	 */
	public Group3DSphere(int loRes, int laRes) {
		longRes = loRes;
		latRes = laRes;

		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Alternate constructor: specify resolutions and radius
	 */
	public Group3DSphere(int loRes, int laRes, int rad) {
		longRes = loRes;
		latRes = laRes;
		radius = rad;

		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Creates the geometry.
	 * 
	 * @return the geometry
	 */
	private Geometry createGeometry() {
		// create line for X axis
		int nVertex = (longRes * (latRes - 2)) + 2;
		int nLines = 4 * longRes * (latRes + 1);
		IndexedLineArray longitudes = new IndexedLineArray(nVertex,
				GeometryArray.COORDINATES, nLines);

		longitudes.setCoordinate(0, new Point3d(0.0d, 0.0d, radius));
		longitudes.setCoordinate(1, new Point3d(0.0d, 0.0d, -radius));

		int i;
		int j;
		int cc = 2;
		int ci = 0;
		double phi0 = Math.PI / longRes;
		double theta0 = (2.0 * Math.PI) / latRes;
		double phi;
		double theta;
		double x;
		double y;
		double z;

		for (i = 0; i < longRes; i++) {
			phi = i * phi0;
			longitudes.setCoordinateIndex(ci++, 0);

			for (j = 1; j < latRes; j++) {
				if (j == (latRes / 2)) {
					longitudes.setCoordinateIndex(ci++, 1);
					longitudes.setCoordinateIndex(ci++, 1);

					continue;
				}

				theta = j * theta0;
				x = radius * Math.sin(theta) * Math.cos(phi);
				y = radius * Math.sin(theta) * Math.sin(phi);
				z = radius * Math.cos(theta);

				longitudes.setCoordinate(cc, new Point3d(x, y, z));
				longitudes.setCoordinateIndex(ci++, cc);
				longitudes.setCoordinateIndex(ci++, cc++);
			}
			longitudes.setCoordinateIndex(ci++, 0);
		}

		int idx0;

		int idx1;

		int half = 0;

		for (j = 1; j < latRes; j++) {
			if (j == (latRes / 2)) {
				continue;
			}

			if (j > (latRes / 2)) {
				half = 1;
			} else {
				half = 0;
			}

			for (i = 0; i < longRes; i++) {
				idx0 = ((i * (latRes - 2)) + j) - half + 1;
				idx1 = idx0 + (latRes - 2);

				if (idx1 >= nVertex) {
					idx1 = latRes - j + half;
				}

				longitudes.setCoordinateIndex(ci++, idx0);
				longitudes.setCoordinateIndex(ci++, idx1);
			}
		}

		return longitudes;
	}

	/**
	 * Creates the appearance.
	 * 
	 * @return the appearance
	 */
	private Appearance createAppearance() {
		Appearance app;
		app = new Appearance();

		return app;
	}
}
