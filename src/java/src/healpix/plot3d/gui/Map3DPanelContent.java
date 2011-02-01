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
package healpix.plot3d.gui;

import healpix.core.dm.HealpixMap;
import healpix.plot3d.canvas3d.MapCanvas;
import healpix.tools.HealpixMapCreator;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;

/**
 * Simple panel containing the canvas 3d map. Can be used from outside plotting
 * GUI.
 * 
 * @author ejoliet
 * @version $Id: Map3DPanelContent.java 56224 2008-07-30 07:30:00Z ejoliet $
 */
public class Map3DPanelContent extends JPanel implements ActionListener, MapTaker {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/**
	 * Map canvas 3d
	 */
	public MapCanvas canvas3Dmap;

	/**
	 * The map to be displayed
	 */
	private HealpixMap map;

	/**
	 * Construct the map viewer adding the canvas 3d map to the simple universe.
	 * 
	 */
	public Map3DPanelContent() {
		super();
		start();
	}

	/**
	 * Start.
	 */
	public void start() {
		setLayout(new BorderLayout());
		// canvas3Dmap = createSimpleUniverse();
		addCanvas(createSimpleUniverse());
	}

	/**
	 * Creates simple universe for display
	 * 
	 * @return tha map canvas
	 */
	public MapCanvas createSimpleUniverse() {
		canvas3Dmap = new MapCanvas(false,0.2f);
		canvas3Dmap.setupScene();
		canvas3Dmap.showScene();
		return canvas3Dmap;
	}

	/**
	 * Adds the canvas.
	 * 
	 * @param m the m
	 */
	private void addCanvas(MapCanvas m) {
		add(m, BorderLayout.CENTER);
	}

	/**
	 * Generates the map to be displayed
	 * 
	 */
	public void generateMap() {
		HealpixMapCreator cr = new HealpixMapCreator("data/test/test_nsl_64.ds");
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
		setMap(m, 0);
	}

	/**
	 * Sets the healpix ith map to be shown
	 * 
	 * @param m
	 *            the map
	 * @param imap
	 *            the ith map
	 */
	public void setMap(HealpixMap m, int imap) {
		System.out.println("Setting the map:" + m.toString());
		canvas3Dmap.setMap(m, imap);
		map = m;
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
	}
}
