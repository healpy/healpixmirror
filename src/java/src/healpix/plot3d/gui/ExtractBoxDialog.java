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

import java.awt.Button;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Extract map dialog.
 * 
 * @author ejoliet
 * @version $Id: ExtractBoxDialog.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class ExtractBoxDialog extends Frame implements ActionListener {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The map. */
	protected HealpixMap map;

	/** The lat. */
	protected TextField lat;

	/** The lon. */
	protected TextField lon;

	/** The width. */
	protected TextField width;

	/** The displayer. */
	protected ExtBoxDisplayer displayer;

	/**
	 * Constructor with external display as input.
	 * 
	 * @param displayer
	 *            external display
	 */
	public ExtractBoxDialog(ExtBoxDisplayer displayer) {
		super("Box Extraction Dialog");
		this.displayer = displayer;
		setSize(300, 160);
		Label latL = new Label("Latitude [-90.0, 90.0]");
		Label lonL = new Label("Longitude [0, 360.0]");
		Label widthL = new Label("Box width (# of pixels)");

		lat = new TextField("     ");
		lon = new TextField("    ");
		width = new TextField("10");
		Panel inpPanel = new Panel(new GridLayout(3, 2));
		inpPanel.add(lonL);
		inpPanel.add(lon);
		inpPanel.add(latL);
		inpPanel.add(lat);
		inpPanel.add(widthL);
		inpPanel.add(width);

		Button close = new Button("Close");
		Button display = new Button("Display");

		Panel p = new Panel();
		p.add(close);
		p.add(display);

		close.addActionListener(this);
		display.addActionListener(this);

		add("Center", inpPanel);
		add("South", p);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		String cmd = e.getActionCommand();
		if (cmd == "Close") {
			setVisible(false);
			return;
		}
		if (cmd == "Display") {
			try {
				int w = Integer.parseInt(width.getText());
				int xsize = w;
				double[] prouni = map.mollpro(w, 0.f);
				// int[][] theBox = map.box(centrePix, w, w);
				int ysize = (int) (xsize / 2);
				double[][] pro = new double[ysize][xsize];
				for (int i = 0; i < xsize; i++) {
					for (int j = 0; j < ysize; j++) {
						int t = ysize - 1 - j;
						pro[t][i] = prouni[i + xsize * j];
					}
				}
				// got the box let's display it
				displayer.setPixBox(pro);

			} catch (Exception ex) {
				ex.printStackTrace();
			}

			return;
		}

	}

	/**
	 * Sets the map.
	 * 
	 * @param m the new map
	 */
	public void setMap(HealpixMap m) {
		this.map = m;
	}
}
