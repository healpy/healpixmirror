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
package healpix.plot3d.progs;

import healpix.core.dm.HealpixMap;
import healpix.plot3d.gui.view.MapView3d;
import healpix.tools.HealpixMapCreator;

import java.io.File;

/**
 * The main priogram to run the healpix 3d map viewer
 * 
 * Usage: java -cp jhealpix.jar healpix.plot3d.progs.Healpix3DMapViewer
 * [filename]
 * 
 * @author ejoliet
 * @version $Id: Healpix3DMapViewer.java 131498 2010-04-15 13:56:00Z womullan $
 */
public class Healpix3DMapViewer {
	
	/**
	 * The main method.
	 * 
	 * @param args the arguments
	 * 
	 * @throws Exception the exception
	 */
	public static void main(String[] args) throws Exception {
		String name = "data/test/test_nsl_64.ds";
		if (args.length < 1) {
			System.err
					.println("Warning: no filename given as argument, using default");
		} else {
			name = args[0];
		}
		try {
			MapView3d mview = new MapView3d(false);
			if (new File(name).exists()) {
				mview.setPath(name);
			} else {
				System.err.println("Error: File \"" + name
						+ "\" does not exist.");
				System.exit(1);
			}
			HealpixMapCreator cr = new HealpixMapCreator(name);
			HealpixMap map = cr.getMap();

			System.out.println(name + " is read!");
			mview.info1.setText(new File(name).getAbsolutePath() + " [Nside="
					+ map.nside() + "]");
			if (map != null) {
				mview.setMap(map);
				System.out.println("Map min/max: " + map.getMin(0) + "/"
						+ map.getMax(0));
			}
			mview.setSize(800, 800);
			mview.setVisible(true);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
