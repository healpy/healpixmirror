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
 * A circle shape, extending Shape3D, drawn in the x,y plane around the origin
 * Radius and number of points that make up the circle may be set
 * 
 * @author ejoliet
 * @version $Id: Group3DCircle.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class Group3DCircle extends Shape3D {
	
	/** The resolution. */
	private int resolution = 24; // default res

	/** The radius0. */
	private double radius0 = 1.0;

	/**
	 * Create default circle visual object
	 */
	public Group3DCircle() {
		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Creates a new Group3DCircle object setting a radius.
	 * 
	 * @param radius
	 *            the actual circle radius
	 */
	public Group3DCircle(double radius) {
		radius0 = radius;

		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Creates a new Group3DCircle object setting the circle radius and its
	 * resolution.
	 * 
	 * @param radius
	 *            circle radius
	 * @param res
	 *            circle visual resolution
	 */
	public Group3DCircle(double radius, int res) {
		radius0 = radius;
		resolution = res;

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
		int nVertex = resolution;
		int nLines = 2 * resolution;
		IndexedLineArray circle = new IndexedLineArray(nVertex,
				GeometryArray.COORDINATES, nLines);

		int i;
		int cc = 1;
		int ci = 0;
		double phi0 = (2.0 * Math.PI) / resolution;
		double phi;
		double x;
		double y;

		circle.setCoordinate(0, new Point3d(radius0, 0.0, 0.0));
		circle.setCoordinateIndex(ci++, 0);

		for (i = 1; i < resolution; i++) {
			phi = i * phi0;

			x = radius0 * Math.cos(phi);
			y = radius0 * Math.sin(phi);
			circle.setCoordinate(cc, new Point3d(x, y, 0));

			circle.setCoordinateIndex(ci++, cc);
			circle.setCoordinateIndex(ci++, cc++);
		}

		circle.setCoordinateIndex(ci++, 0);

		return circle;
	}

	/**
	 * Creates the appearance.
	 * 
	 * @return the appearance
	 */
	private Appearance createAppearance() {
		Appearance app = new Appearance();

		return app;
	}
}
