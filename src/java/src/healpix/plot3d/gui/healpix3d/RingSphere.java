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

import healpix.core.Healpix;
import healpix.tools.Constants;
import healpix.tools.SpatialVector;

import javax.media.j3d.Appearance;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.LineArray;

/**
 * Creates a Ring Healpix tesselasition
 * 
 * @author ejoliet
 * @version $Id: RingSphere.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class RingSphere extends HealSphere {

	/** The ring. */
	protected int ring = 6;

	/**
	 * create sphere visual object
	 */
	public RingSphere() {
		super();
		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/**
	 * Instantiates a new ring sphere.
	 * 
	 * @param nside the nside
	 * @param ring the ring
	 */
	public RingSphere(int nside, int ring) {
		super(nside);
		this.ring = ring;
		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
	 */
	protected Geometry createGeometry() {
		double nms[], theta_center, phi_center;
		// double thn, ths;
		int ppq = (step * 2 + 2) * 2; // points per quad
		int i_phi_count, rpix;
		int ns4 = 4 * nside;
		int ns3 = 3 * nside;
		int q = 0;
		int i_th = this.ring;
		if (i_th <= 0)
			i_th = 1;
		i_phi_count = Math.min(i_th, Math.min(nside, ns4 - i_th));
		int nPoints = i_phi_count * ppq * 4;
		LineArray quads = new LineArray(nPoints, GeometryArray.COORDINATES);
		// DecimalFormat form = new DecimalFormat("0.000");
		// DecimalFormat fi = new DecimalFormat("000");
		// System.out.println("rpix costh thn ths theta phi phil phir i_th i_phi
		// npix- Ring");

		nms = Healpix.integration_limits_in_costh(nside, i_th);
		theta_center = Math.acos(nms[1]);
		// thn = Math.acos(nms[0]);
		// ths = Math.acos(nms[2]);
		// all 4 zones for this one ring
		for (int i_zone = 0; i_zone < 4; i_zone++)
			for (int i_phi = 1; i_phi <= i_phi_count; i_phi++) {
				if (i_th >= nside && i_th <= ns3) {
					phi_center = ((double) (i_zone * nside + i_phi) - (((i_th % 2) + 1) / 2.0))
							* Constants.PI / 2.0 / (double) nside;
				} else {
					phi_center = ((i_zone * i_phi_count + i_phi) - 0.5)
							* Constants.PI / 2.0 / (double) i_phi_count;
				}
				try {
					rpix = Healpix
							.ang2pix_ring(nside, theta_center, phi_center);
					// int npix =
					// Healpix.ang2pix_nest(nside,theta_center,phi_center);
					int offset = q * ppq;
					q++;
					SpatialVector corners[] = index.corners_ring(rpix, step);
					addPix(corners, offset, quads);

				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		return quads;
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createAppearance()
	 */
	protected Appearance createAppearance() {
		Appearance app;
		app = new Appearance();
		ColoringAttributes ca = new ColoringAttributes();
		ca.setColor(1.0f, 0.1f, 0.10f);
		app.setColoringAttributes(ca);

		return app;
	}

}
