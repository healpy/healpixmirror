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

import healpix.essentials.Pointing;
import healpix.essentials.Scheme;
import healpix.tools.Constants;
import healpix.essentials.Vec3;

import javax.media.j3d.Appearance;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.Geometry;
import javax.media.j3d.GeometryArray;
import javax.media.j3d.LineArray;

/**
 * Part of the {@link HealSphere}
 *
 * @author ejoliet
 * @version $Id: ZoneSphere.java 114558 2009-12-01 17:02:57Z ejoliet $
 */
public class ZoneSphere extends HealSphere {

	/** The i_zone. */
	protected int i_zone = 0;

	/**
	 * Instantiates a new zone sphere.
	 *
	 * @param nside the nside
	 * @param zone the zone
	 */
	public ZoneSphere(int nside, int zone) {
		super(nside,Scheme.RING);
		this.i_zone = zone;

		this.setGeometry(createGeometry());
		this.setAppearance(createAppearance());
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.healpix3d.HealSphere#createGeometry()
	 */
	protected Geometry createGeometry() {
		double theta_center, phi_center;
		// double thn, ths;
		// double philr[];
		int ppq = step*8; // points per quad
		int i_phi_count, rpix;
		int ns4 = 4 * nside;
		int ns3 = 3 * nside;
		int nQuads = (12 / 4) * (nside * nside);
		int nPoints = nQuads * ppq;
		int q = 0;
		LineArray quads = new LineArray(nPoints, GeometryArray.COORDINATES);
		// DecimalFormat form = new DecimalFormat("0.000");
		// DecimalFormat fi = new DecimalFormat("000");
		// System.out.println("rpix costh thn ths theta phi phil phir i_th i_phi
		// - Zone");

		for (int i_th = 1; i_th < ns4; i_th++) {
			theta_center = Math.acos(index.ring2z(i_th));
			// thn = Math.acos(nms[0]);
			// ths = Math.acos(nms[2]);
			i_phi_count = Math.min(i_th, Math.min(nside, ns4 - i_th));
			// should do the zone just do zero for now
			// int i_zone =1;
			// for (int i_zone =0; i_zone < 4; i_zone++)
			for (int i_phi = 1; i_phi <= i_phi_count; i_phi++) {
				if (i_th >= nside && i_th <= ns3) {
					phi_center = ((double) (i_zone * nside + i_phi) - (((i_th % 2) + 1) / 2.0))
							* Constants.PI / 2.0 / (double) nside;
				} else {
					phi_center = ((i_zone * i_phi_count + i_phi) - 0.5)
							* Constants.PI / 2.0 / (double) i_phi_count;
				}
				try {
					rpix = (int)index.ang2pix(new Pointing(theta_center, phi_center));
					// int npix =
					// Healpix.ang2pix_nest(nside,theta_center,phi_center);
					// philr = Healpix.pixel_boundaries(nside, i_th, i_phi,
					// i_zone, nms[1]);

					int offset = q * ppq;
					q++;
					Vec3[] corners = index.boundaries(rpix, step);
					addPix(corners, offset, quads);
				} catch (Exception e) {
					e.printStackTrace();
				}
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
		ca.setColor(0.2f, 0.3f, 0.80f);
		app.setColoringAttributes(ca);

		return app;
	}

}
