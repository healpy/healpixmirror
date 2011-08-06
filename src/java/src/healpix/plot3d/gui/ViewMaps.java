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
import healpix.tools.HealpixMapCreator;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Canvas;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;

import javax.swing.JApplet;

/**
 * Allow map selection and eventually display it and allow zoom.
 * 
 * @author ejoliet
 * @version $Id: ViewMaps.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class ViewMaps extends JApplet implements ActionListener, TextListener,
		ItemListener, MapTaker {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The stoped. */
	protected boolean stoped = true;

	/** The started. */
	protected boolean started = false;

	/** The view3dfailed. */
	protected boolean view3dfailed = false;

	/** The view area. */
	protected Panel viewArea;

	/** The info. */
	protected Panel info;

	/** The info2. */
	protected Label info2;

	/** The width. */
	protected Label width;

	/** The threshold. */
	protected Label threshold;

	/** The ra field. */
	protected TextField raField;

	/** The dec field. */
	protected TextField decField;

	/** The info3. */
	protected Label info3;

	/** The map. */
	protected HealpixMap theMap;

	/** The map dlg. */
	protected MapDialog mapDlg;

	/** The box dlg. */
	protected ExtractBoxDialog boxDlg;

	/** The displayer. */
	protected ExtBoxDisplayer displayer;

	/** The map pan. */
	protected MapPanel mapPan;

	/** The view3d. */
	protected MapTaker view3d;

	/** The show3d. */
	protected Button extractBox, show3d;

	/** The cr. */
	static HealpixMapCreator cr;

	/**
	 * TODO Should be in progs package but: Use the extract box in the progs and remove this class.
	 */
	public ViewMaps() {
	}

	/**
	 */
	public static void main(String argv[]) {
		ViewMaps ap = new ViewMaps();
		new MainFrame(ap, null, 500, 350);
	}

	/**
	 * Window manager sets the size of the window officially but we can suggest
	 * which size we would like
	 */
	public Dimension getPreferredSize() {
		return new Dimension(800, 350);
	}

	/**
	 * This does all the setup and layout
	 */
	public void init() {
		cr = new HealpixMapCreator("data/test/test_2.ds");
		setLayout(new BorderLayout());
		info2 = new Label("                           ");
		viewArea = new Panel();
		info = new Panel();
		info.setLayout(new BorderLayout());

		displayer = new BoxDisplay();
		// controls
		Panel controlp = new Panel();

		Button select = new Button("Select Map");
		extractBox = new Button("Extract Box");
		show3d = new Button("Show Map 3d");
		controlp.add("East", select);
		controlp.add("East", extractBox);
		controlp.add("East", show3d);
		select.addActionListener(this);
		extractBox.addActionListener(this);
		show3d.addActionListener(this);
		extractBox.setEnabled(false);
		show3d.setEnabled(true);

		mapDlg = new MapDialog(this);
		mapPan = new MapPanel();

		// view3d = new planck.gui.healpix3d.MapView3d();
		// checking if can make MapView3d (ie java 3d library is available)
		try {
			// how to make a class at run time - rather than at compile time:
			view3d = (MapTaker)Class.forName("healpix.plot3d.gui.view.MapView3d").newInstance();
		} catch (Exception ex) {
			show3d.setEnabled(false);
			view3dfailed = true;
			System.err.println("Java 3d library not found!");
			ex.printStackTrace();
		}
		setMap(cr.getMap());
		GridBagLayout gridbag = new GridBagLayout();
		Panel rp = new Panel();
		rp.setLayout(gridbag);
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.BOTH;
		cons.gridy = GridBagConstraints.RELATIVE;
		cons.gridx = 0;
		cons.ipadx = 0;
		cons.ipady = 2;
		gridbag.setConstraints(mapPan, cons);
		rp.add(mapPan);
		add("East", rp);
		add("Center", (Canvas) displayer);
		info.add("Center", controlp);
		info.add("South", info2);
		add("South", info);
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.MapTaker#setMap(healpix.core.dm.HealpixMap)
	 */
	public void setMap(HealpixMap map) {
		// Update the MapPanel and the rest
		theMap = map;
		mapPan.setMap(map);
		if (!view3dfailed)
			show3d.setEnabled(true);
		if (view3d != null) {
			view3d.setMap(map);
		}
		if (extractBox != null) {
			extractBox.setEnabled(true);
		}
		if (boxDlg != null)
			boxDlg.setMap(map);
	}

	/**
	 */
	public String getInfo() {
		return " Map viewer for DMC  by William O'Mullane ";
	}

	/**
	 * This is required to implement ActionListener - this gets called when
	 * someone hits a button
	 */
	public void actionPerformed(ActionEvent e) {
		info2.setText(" ");
		String label = e.getActionCommand();
		System.out.println("Button is " + label);
		if (label == "View") {
		}// end if Button is View
		if (label == "Apply") {
		}// end if Button is Apply
		if (label == "Select Map") {
			mapDlg.setVisible(true);
		}// end if Button is select
		if (label == "Extract Box") {
			if (boxDlg == null)
				boxDlg = new ExtractBoxDialog(displayer);
			if (view3d != null)
				setMap(view3d.getMap());
			boxDlg.setMap(theMap);
			boxDlg.setVisible(true);
		}// e end if Button is view 3d
		if (label == "Show Map 3d") {

			if (view3d != null) {
				Component vi = (Component) view3d;
				vi.setVisible(true);
				// vi.show();
				// view3d.setMap(theMap);
			}
		}
	}

	/**
	 * This must be imlpmented to satify the TextListener interface it gets
	 * called when the text in the box changes
	 */
	public void textValueChanged(TextEvent evt) {
	}

	/**
	 * Item listener imp - for check boxes
	 */
	public void itemStateChanged(ItemEvent evt) {
		info2.setText(" ");
	}

	/* (non-Javadoc)
	 * @see healpix.plot3d.gui.MapTaker#getMap()
	 */
	public HealpixMap getMap() {
		return theMap;
	}
}
