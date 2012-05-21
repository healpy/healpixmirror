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

import healpix.essentials.Vec3;
import healpix.essentials.Scheme;

import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.QuadArray;
import javax.vecmath.Point3d;
/**
 * Creates a Healpix Face tesselation
 *
 * @author ejoliet
 * @version $Id: FaceSphere.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class FaceSphere extends HealSphere {

	/** The face. */
	protected int face = 0;

	/**
	 * Instantiates a new face sphere.
	 *
	 * @param nside the nside
	 * @param face the face
	 */
	public FaceSphere(int nside, int face) {
		super(nside,Scheme.NESTED);
		this.face = face;
		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/* Render the given face to given resolution */
	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
	 */
	protected Geometry createGeometry() {
		int nQuads = (int) Math.pow(nside, 2);// one face
		int ppq = 4;// points per quad
		int nPoints = nQuads * ppq;
		int faceoff = nQuads * face;
		QuadArray quads = new QuadArray(nPoints, GeometryArray.COORDINATES);

		try {
			int offset;
			for (int q = 0; q < nQuads; q++) {
				Vec3[] points = index.boundaries(faceoff + q, 1);
				offset = q * ppq;
				// need to add pixel

				for (int v = 0; v < points.length; v++) {
					quads.setCoordinate(offset + v, new Point3d(points[v].x,
							points[v].y, points[v].z));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return quads;
	}

}
