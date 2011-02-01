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
package healpix.plot3d.gui.view;

import healpix.plot3d.gui.healpix3d.HealCanvas;
import healpix.plot3d.gui.healpix3d.HealPanel;
import healpix.plot3d.gui.healpix3d.RotatePanel;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Label;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * View the healpix pixelisation of the sphere, also with solid pixels. A viewer
 * for healpix explanation purposes but can be used to extend a map viewer
 * feature.
 * 
 * @author ejoliet
 * @version $Id: HealView.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class HealView extends Frame implements ActionListener {

    /** The Constant serialVersionUID. */
    private static final long serialVersionUID = 1L;

	/** The sky. */
	private HealCanvas theSky;

	/** The info1. */
	private Label info1;

	/** The rot panel. */
	protected RotatePanel rotPanel;

	/** Default constructor - Sets name */
	public HealView() {
		super("Healpix 3D");
	}

	/**
	 * Window manager sets the size of the window officially but we can suggest
	 * which size we would like
	 */
	public Dimension getPreferredSize() {
		return new Dimension(750, 680);
	}

	/** init lays out the window and sets up components */
	public void init() {
		/*
		 * BorderLayout just makes sure the Object in the Center gets most of
		 * the screen
		 */
		setLayout(new BorderLayout());
		// Information area where we can put some text occasionally
		info1 = new Label("Welcome !");
		// put it along the top
		add("North", info1);

		// Plotting area - create our own sky class
		theSky = new HealCanvas();
		// put it in the center of the window - most space
		add("Center", theSky);
		theSky.setupScene();
		theSky.showScene();

		// Create a panel to put the controls in - panel is a long strip
		FlowLayout fl = new FlowLayout(FlowLayout.CENTER, 1, 1);
		Panel p = new Panel(fl);

		// Create buttons and add this as Actionlistener - add them to panel
		Button quit = new Button("Quit");
		quit.addActionListener(this);
		p.add("West", quit);
		// put the panel at the bottom of the screen
		add("South", p);

		Panel rp = new Panel();
		rp.setBackground(Color.black);

		// rp.setLayout(new GridLayout(3,1));
		GridBagLayout gridbag = new GridBagLayout();
		rp.setLayout(gridbag);
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.BOTH;
		cons.gridy = GridBagConstraints.RELATIVE;
		cons.gridx = 0;
		cons.ipadx = 0;
		cons.ipady = 10;

		HealPanel hpan = new HealPanel();
		hpan.setCanvas(theSky);
		gridbag.setConstraints(hpan, cons);
		rp.add(hpan);
		// Create control panel for rotations
		rotPanel = new RotatePanel();
		gridbag.setConstraints(rotPanel, cons);
		rp.add(rotPanel);
		// attach the rotPanel panel to the sky
		rotPanel.setScene(theSky);
		// add to right of screen
		this.add("East", rp);
		rotPanel.start();

	};

	/**
	 * This is required to implement ActionListener - this gets called when
	 * someone hits a button
	 */
	public void actionPerformed(ActionEvent e) {
		String label = e.getActionCommand();
		if (label.equals("Quit")) {
			dispose();
		}
	}
}
