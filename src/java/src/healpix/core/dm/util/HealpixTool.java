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
package healpix.core.dm.util;

import healpix.core.dm.HealpixMap;
import healpix.core.dm.HealpixMapImp;
import healpix.essentials.Scheme;

/**
 * Tools (Regrade) to apply to healpix maps. Could be extended with other tool
 * like comparing maps, combine maps,etc.
 *
 * @author ejoliet
 * @version $Id: HealpixTool.java 135547 2010-05-13 13:40:26Z womullan $
 */
public class HealpixTool {
	/**
	 * Input {@link HealpixMap}
	 */
	public HealpixMap map_in;

	/**
	 * Output {@link HealpixMap} result
	 */
	public HealpixMap map_out;

	/**
	 * Creates the tool to regrade an input map
	 *
	 * @param min
	 *            input map {@link HealpixMap}
	 */
	public HealpixTool(HealpixMap min) {
		this.map_in = min;
		// this.map_out = mout;
	}

	/**
	 * Default constructor
	 *
	 */
	public HealpixTool() {
	}

	/**
	 * Upgrade a healpix map from nside_in to nside_out (>nside_n)
	 *
	 * @param nside_out
	 *            The nside of the resulting healpix map upgraded (>nside_in)
	 * @return The {@link HealpixMap} upgraded.
	 * @throws Exception
	 */
	public HealpixMap upgrade(int nside_out) throws Exception {
		if (map_in.getScheme().equals(Scheme.NESTED)) { // NEST
			upgrade_nest(nside_out);
		} else { // RING
			upgrade_ring(nside_out);
		}

		return map_out;
	}

	/**
	 * Upgrade a RING healpix map.
	 *
	 * @param nside_out
	 *            The output resolution.
	 * @throws Exception
	 */
	private void upgrade_ring(int nside_out) throws Exception {
		map_out = new HealpixMapImp(12 * nside_out * nside_out, map_in
				.getName());
		map_out.setScheme(map_in.getScheme());
		map_in.convert_ring2nest();
		// ***Upgrade as NEST healpix map.
		upgrade_nest(nside_out);
		map_out.convert_nest2ring();
		setMapOut(map_out);

	}

	/**
	 * Upgrade a NEST healpix map to nside_out resolution.
	 *
	 * @param nside_out
	 *            The output resolution.
	 * @throws Exception
	 */
	private void upgrade_nest(int nside_out) throws Exception {
		map_out = new HealpixMapImp(12 * nside_out * nside_out, map_in
				.getName());
		map_out.setScheme(map_in.getScheme());
		double npix_out = map_out.nPixel();
		double npix_in = map_in.nPixel();
		double npratio = npix_out / npix_in;
		System.out.println("Npix in:" + npix_in);
		System.out.println("Npix out:" + npix_out);
		System.out.println("npix_out/npix_in=" + npratio);
		int ip = 0;
		for (int nmap = 0; nmap < map_in.getName().length; nmap++) {
			for (int iu = 0; iu < npix_out; iu++) {
				ip = (int) (iu / npratio);
				map_out.setValueCell(nmap, iu, map_in.get(nmap, ip));
			}
		}
		setMapOut(map_out);

	}

	/**
	 * Degrade the {@link HealpixMap} to Nside_out.
	 *
	 * @param nside_out
	 *            The NSIDE output resolution.
	 * @return The {@link HealpixMap} degraded.
	 * @throws Exception
	 */
	public HealpixMap degrade(int nside_out) throws Exception {
		map_out = new HealpixMapImp(12 * nside_out * nside_out, map_in
				.getName());
		map_out.setScheme(map_in.getScheme());
		double npix_out = map_out.nPixel();
		double npix_in = map_in.nPixel();
		double npratio = npix_in / npix_out;
		System.out.println("Npix in:" + npix_in);
		System.out.println("Npix out:" + npix_out);
		System.out.println("npix_out/npix_in=" + npratio);
		for (int nmap = 0; nmap < map_in.getName().length; nmap++) {
			for (int id = 0; id < npix_out; id++) {
				int firstPix = (int) (id * npratio);
				int lastPix = (int) ((id + 1) * npratio - 1);
				map_out.setValueCell(nmap, id, map_in.mean(nmap, firstPix,
						lastPix));
			}
		}
		setMapOut(map_out);
		return map_out;
	}

	/**
	 * Sets the output result {@link HealpixMap}
	 *
	 * @param mout
	 *            output map
	 */
	public void setMapOut(HealpixMap mout) {
		System.out.println("********** Returned Map Nside=" + mout.nside());
		System.out.println("********** Returned Map N pixels=" + mout.nPixel());
		this.map_out = mout;
	}

	/**
	 * Sets the input {@link HealpixMap}
	 *
	 * @param min
	 *            the input map
	 */
	public void setMapIn(HealpixMap min) {
		this.map_in = min;
	}
}
