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
package healpix.plot3d.demo;

import healpix.core.dm.HealpixMap;
import healpix.tools.HealpixMapCreator;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.UIManager;

import com.sun.j3d.utils.universe.SimpleUniverse;

/**
 * Simple JFrame to demostrate the healpix map 3d viewer. Can interact with the
 * mouse buttons.
 * 
 * @author ejoliet
 * @version $Id: Healpix3DMap.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class Healpix3DMap extends JFrame {
	/**
	 * Default serial version UID
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The title to appear in the frame.
	 */
	private static final String FRAME_TITLE = "Healpix 3D Map Viewer";

	/**
	 * The window default WIDTH
	 */
	private static final int FRAME_WIDTH = 800;

	/**
	 * The window default HEIGHT
	 */
	private static final int FRAME_HEIGHT = 600;

	/**
	 * Map canvas 3d
	 */
	MapCanvas3D canvas3Dmap;

	/**
	 * The map to be displayed
	 */
	private HealpixMap map;

	/**
	 * Flag indicating if the application has been launched as an applet.
	 */
	private boolean isApplet = false;

	/**
	 * Flag indicating when the user exits the application.
	 */
	private boolean isDone = false;

	/**
	 * Construct the map viewer adding the canvas 3d map to the simple universe.
	 * 
	 * @param isApplet
	 *            start as applet if true
	 */
	public Healpix3DMap(boolean isApplet) {
		super(FRAME_TITLE);
		this.isApplet = isApplet;
		setupFrame();
		start();
	}

	/**
	 * Default constructor. Not an applet, it is a JFrame application.
	 */
	public Healpix3DMap() {
		this(false);
	}

	/**
	 * Setup frame.
	 */
	private void setupFrame() {
		// Set cross-platform Java L&F (also called "Metal")
		try {
			UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
		} catch (Exception e) {
			new Exception("Failed to set Metal Look and Feel" + e);
		}

		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				exitApplication();
			}
		});

		int width = Toolkit.getDefaultToolkit().getScreenSize().width;
		int height = Toolkit.getDefaultToolkit().getScreenSize().height;
		int frameWidth = Math.min(FRAME_WIDTH, width - 100);
		int frameHeight = Math.min(FRAME_HEIGHT, height - 100);
		setSize(new Dimension(frameWidth, frameHeight));
		setLocation((width - frameWidth) / 2, ((height - frameHeight) / 2));
	}

	/**
	 * Setting up the layout and adding the universe
	 */
	public void start() {
		setLayout(new BorderLayout());
		// canvas3Dmap = createSimpleUniverse();
		addCanvas(createSimpleUniverse());
		pack();
		// setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}

	/**
	 * Creates simple universe for display
	 * 
	 * @return the 3d canvas prepared to display the {@link HealpixMap} map
	 */
	public MapCanvas3D createSimpleUniverse() {
		GraphicsConfiguration config = SimpleUniverse
				.getPreferredConfiguration();
		canvas3Dmap = new MapCanvas3D(config);
		// canvas3Dmap.setShowGrid(false);
		canvas3Dmap.setupScene();
		canvas3Dmap.showScene();
		return canvas3Dmap;
	}

	/**
	 * Adding to the {@link MapCanvas3D} to the JFrame content
	 * 
	 * @param m
	 *            {@link MapCanvas3D} input
	 */
	private void addCanvas(MapCanvas3D m) {
		add("Center", m);
	}

	/**
	 * Generates the dummy data Healpix map to be displayed
	 * 
	 */
	public void generateMap() {
		HealpixMapCreator cr = new HealpixMapCreator();
		map = cr.getMap();
		System.out.println("********** Map nside=" + map.nside());
		System.out.println("********** Map pixels=" + map.nPixel());
		setMap(map);
	}

	/**
	 * Gets the healpix map
	 * 
	 * @return the map
	 */
	public HealpixMap getMap() {
		return map;
	}

	/**
	 * Sets the healpix map to be shown
	 * 
	 * @param m
	 *            the map
	 */
	public void setMap(HealpixMap m) {
		System.out.println("Setting the map:" + m.toString());
		canvas3Dmap.setMap(m);
		map = m;
	}

	/**
	 * Runs the demo.
	 * 
	 * @param args
	 *            arguments if any
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		try {
			HealpixMapCreator cr = new HealpixMapCreator("data/test/test_2.ds");
			HealpixMap map = cr.getMap();
			System.out.println("********** Map nside=" + map.nside());
			System.out.println("********** Map pixels=" + map.nPixel());
			Healpix3DMap hpix = new Healpix3DMap();
			hpix.start();
			hpix.setMap(map);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Checks if is done.
	 * 
	 * @return true, if is done
	 */
	public boolean isDone() {
		return isDone;
	}

	/**
	 * Method to exit the application, set the isDone flag to true.
	 * 
	 */
	private void exitApplication() {
		setVisible(false);
		isDone = true;
		if (!isApplet) {
			System.exit(0);
		}
	}
}
