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
package healpix.plot3d.gui.healpix3d;

import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Choice;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.ItemSelectable;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

/**
 * The Class HealPanel.
 */
public class HealPanel extends Panel implements ItemListener, KeyListener {
	/**
	 * Default serial version
	 */
	private static final long serialVersionUID = 1L;

	/** The canvas. */
	protected HealCanvas canvas;

	/** The nside. */
	protected Choice nside;

	/** The solidface. */
	protected Checkbox zone, nest, ring, axis, face, rings, solidface;

	/** The nchoice. */
	protected Choice zchoice, nchoice;

	/** The ring field. */
	protected TextField ringField;

	/** initialiser */
	protected void init() {
		GridBagLayout gridbag = new GridBagLayout();
		setLayout(gridbag);
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridy = GridBagConstraints.RELATIVE;
		c.gridx = 0;
		c.ipadx = 0;
		c.ipady = 0;
		add(new Label("Healpix"));
		nside = new Choice();
		nside.setForeground(Color.blue);
		nside.addItem("1");
                nside.addItem("2");
		nside.addItem("4");
		nside.addItem("8");
		nside.addItem("16");
		nside.addItem("32");
		nside.addItem("64");
		nside.addItem("128");
		nside.addItem("256");
		nside.select("8");
		nside.addItemListener(this);
		Panel fac = new Panel();
		fac.setLayout(new GridLayout(1, 2));
		Label delay = new Label("NSIDE:");
		delay.setForeground(Color.blue);
		fac.add(delay);
		fac.add(nside);
		fac.setBackground(Color.white);
		Panel grids = new Panel(new GridLayout(6, 1));
		grids.setBackground(Color.white);
		// CheckboxGroup cbg = new CheckboxGroup();
		CheckboxGroup cbg = null;

		Panel zp = new Panel();
		zone = new Checkbox("Zone", cbg, true);
		zone.setForeground(Color.blue);
		zone.addItemListener(this);
		zchoice = new Choice();
		zchoice.setForeground(Color.blue);
		zchoice.addItem("0");
		zchoice.addItem("1");
		zchoice.addItem("2");
		zchoice.addItem("3");
		zchoice.select("3");
		zchoice.addItemListener(this);
		zp.add(zone);
		zp.add(zchoice);

		rings = new Checkbox("Rings", cbg, false);
		rings.setForeground(Color.orange);
		rings.addItemListener(this);

		face = new Checkbox("Base Pixels", cbg, false);
		face.setForeground(Color.orange);
		face.addItemListener(this);

		solidface = new Checkbox("Solid base pixels", cbg, false);
		solidface.setForeground(Color.orange);
		solidface.addItemListener(this);

		Panel np = new Panel();
		nest = new Checkbox("Nest", cbg, true);
		nest.setForeground(Color.green);
		nest.addItemListener(this);
		nchoice = new Choice();
		nchoice.setForeground(Color.green);
		nchoice.addItem("0");
		nchoice.addItem("1");
		nchoice.addItem("2");
		nchoice.addItem("3");
		nchoice.addItem("4");
		nchoice.addItem("5");
		nchoice.addItem("6");
		nchoice.addItem("7");
		nchoice.addItem("8");
		nchoice.addItem("9");
		nchoice.addItem("10");
		nchoice.addItem("11");
		nchoice.select("0");
		nchoice.addItemListener(this);
		np.add(nest);
		np.add(nchoice);

		Panel rp = new Panel();
		ringField = new TextField("  5");
		ringField.addKeyListener(this);
		ring = new Checkbox("Ring", cbg, true);
		ring.setForeground(Color.red);
		ring.addItemListener(this);
		rp.add(ring);
		rp.add(ringField);

		axis = new Checkbox("Axis", cbg, true);
		axis.setForeground(Color.red);
		axis.addItemListener(this);

		grids.add(zp);
		grids.add(np);
		grids.add(rp);
		grids.add(axis);
		grids.add(face);
		grids.add(rings);
		grids.add(solidface);
		gridbag.setConstraints(grids, c);
		add(grids);
		gridbag.setConstraints(fac, c);
		add(fac);
		setBackground(Color.white);
	}

	/** Set canvas */
	public void setCanvas(HealCanvas canvas) {
		this.canvas = canvas;
	}

	/**
	 * Instantiates a new heal panel.
	 */
	public HealPanel() {
		init();
	}

	/** Item listener imp - for choice box */
	public void itemStateChanged(ItemEvent evt) {
		ItemSelectable is = evt.getItemSelectable();
		if (is == nside) {
			int ch = new Integer(nside.getSelectedItem()).intValue();
			canvas.setNside(ch);
			return;
		}
		if (is == zchoice) {
			int ch = new Integer(zchoice.getSelectedItem()).intValue();
			canvas.setZone(ch);
			zone.setState(true);
			canvas.setViewZone(true);
			return;
		}

		if (is == nchoice) {
			int ch = new Integer(nchoice.getSelectedItem()).intValue();
			canvas.setFace(ch);
			nest.setState(true);
			canvas.setViewNest(true);
			return;
		}

		Checkbox cb = (Checkbox) is;
		if (cb == rings) {
			canvas.setViewRings(cb.getState());
			return;
		}
		if (cb == solidface) {
			canvas.setViewSolidFace(cb.getState());
			return;
		}

		if (cb == face) {
			canvas.setViewFaces(cb.getState());
			return;
		}
		if (cb == axis) {
			canvas.setViewAxis(cb.getState());
			return;
		}
		if (cb == zone) {
			canvas.setViewZone(cb.getState());
			return;
		}
		if (cb == nest) {
			canvas.setViewNest(cb.getState());
			return;
		}
		if (cb == ring) {
			canvas.setViewRing(cb.getState());
			return;
		}

	}

	/* (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
	 */
	public void keyPressed(KeyEvent e) {
	};

	/* (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	public void keyReleased(KeyEvent e) {
	};

	/* (non-Javadoc)
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	public void keyTyped(KeyEvent e) {
		char c = e.getKeyChar();
		if (c == '\n' || c == '\r') {
			int ch = new Integer(ringField.getText().trim()).intValue();
			canvas.setRing(ch);
			ring.setState(true);
			canvas.setViewRing(true);
			return;
		}
	}
};
