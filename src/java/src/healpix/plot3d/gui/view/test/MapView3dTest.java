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
package healpix.plot3d.gui.view.test;

import healpix.core.dm.HealpixMap;
import healpix.plot3d.gui.Map3DPanelContent;
import healpix.plot3d.gui.MapTaker;
import healpix.plot3d.gui.view.MapView3d;
import healpix.tools.HealpixMapCreator;

import java.io.File;

import javax.swing.JFrame;

import junit.framework.TestCase;

/**
 * Test Healpix 3d map viewer.
 * 
 * @author ejoliet
 * @version $Id: MapView3dTest.java 120812 2010-01-24 23:12:12Z ejoliet $
 */
public class MapView3dTest extends TestCase {

	/**
	 * Test plotting a 3d map from a dummy created Healpix map.
	 */
	public void testMap3dViewer() {
		try {
			MapView3d mview = new MapView3d();
			HealpixMapCreator cr = new HealpixMapCreator(1);
			HealpixMap map = cr.getMap();
			if ( map != null ) {
				mview.setMap(map);
				// mview.colorBar.update(new
				// SineColorTransform(map.getMin(0),map.getMax(0)));
				System.out.println("Map min/max: " + map.getMin(0) + "/"
						+ map.getMax(0));
			}
			// mview.setSize(800, 800);
			mview.setVisible(true);
		} catch ( Exception e ) {
			fail("Failure: " + e);
		}
	}

	/**
	 * Test plotting a 3d map from a dummy created Healpix map.
	 */
	public void testMapCanvasContent() {
		try {
			Map3DPanelContent mview = new Map3DPanelContent();
			HealpixMapCreator cr = new HealpixMapCreator(1);
			HealpixMap map = cr.getMap();
			if ( map != null ) {
				mview.setMap(map);
				// mview.colorBar.update(new
				// SineColorTransform(map.getMin(0),map.getMax(0)));
				System.out.println("Map min/max: " + map.getMin(0) + "/"
						+ map.getMax(0));
			}
			JFrame jf = new JFrame();
			jf.add(mview);
			jf.pack();
			jf.setVisible(true);
		} catch ( Exception e ) {
			fail("Failure: " + e);
		}
	}

	/**
	 * Test plotting a 3d map from test fits file.
	 */
	public void test3dViewerFromFits() {
		try {
			String name = "data/test/test_2.ds";
			MapView3d mview = new MapView3d();
			HealpixMapCreator cr = new HealpixMapCreator(name);
			if ( new File(name).exists() )
				mview.setPath(name);
			HealpixMap map = cr.getMap();
			System.out.println(name + " is read!");
			mview.info1.setText(new File(name).getAbsolutePath() + " [Nside="
					+ map.nside() + "]");
			if ( map != null ) {
				mview.setMap(map);
				// mview.colorBar.update(new
				// SineColorTransform(map.getMin(0),map.getMax(0)));
				System.out.println("Map min/max: " + map.getMin(0) + "/"
						+ map.getMax(0));
			}
			// mview.setSize(800, 800);
			// mview.setVisible(true);
		} catch ( Exception e ) {
			fail("Failure: " + e);
		}
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args) {
		int imap = 1;
		MapTaker mview;
		mview = new Map3DPanelContent();
//		mview = new MapView3d(false, false, 0.1f);

		HealpixMapCreator cr = new HealpixMapCreator(8);
		HealpixMap map = cr.getMap();
		map.setImap(imap);
		if ( map != null ) {
			if ( mview instanceof MapView3d ) {
				( (MapView3d) mview ).setMap(map, imap);
			}else {
				mview.setMap(map);
			}

			// mview.colorBar.update(new
			// SineColorTransform(map.getMin(0),map.getMax(0)));
			System.out.println("Map min/max: " + map.getMin(imap) + "/"
					+ map.getMax(imap));
		}
		if ( mview instanceof Map3DPanelContent ) {
			JFrame jf = new JFrame();
			jf.add((Map3DPanelContent)mview);
			jf.setSize(800, 600);
			jf.setVisible(true);
		}
		if ( mview instanceof MapView3d ) {
			( (MapView3d) mview ).setSize(800, 800);
			( (MapView3d) mview ).setVisible(true);
		}
	}
}
