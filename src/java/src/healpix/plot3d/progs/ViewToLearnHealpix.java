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

import healpix.plot3d.gui.view.HealView;

/**
 * Create a new HealView and Display it on the screen, allow the user to get the
 * "healpix experience".
 * 
 * Usage: java -cp jhealpix.jar healpix.plot3d.progs.ViewToLearnHealpix
 * @author ejoliet
 * @version $Id: ViewToLearnHealpix.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class ViewToLearnHealpix {
	
	/**
	 * The main method.
	 * 
	 * @param argv the arguments
	 */
	public static void main(String argv[]) {
		HealView ap = new HealView();
		ap.init();
		ap.pack();
		ap.setVisible(true);
	}
}
