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
package healpix.core.test;

import java.text.DecimalFormat;

import healpix.core.HealpixIndex;


/**
 * just to see if the small jar works with a small program
 * @author womullane
 *
 */
public class TestSmall {

	/**
	 * just to see if the small jar works with a small program
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception  {
		int nside = 262144;
		HealpixIndex hi = new HealpixIndex(nside);
		DecimalFormat form = new DecimalFormat("#.#######");


		double theta = 2.14, phi = 4.28;
		int ind =0;
		if (args.length > ind) {
			theta = Double.parseDouble(args[ind++]);
		}

		if (args.length > ind) {
			phi = Double.parseDouble(args[ind++]);
		}

		long pix = hi.ang2pix_nest(theta, phi);
		double pos[] = hi.pix2ang_nest(pix);
		
		System.out.println("pix="+pix+
				" theta="+form.format(pos[0])+
				" phi="+form.format(pos[1]));
		
	}

}
