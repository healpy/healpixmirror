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

import healpix.plot3d.canvas3d.MapCanvas;

import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Choice;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * JPanel on the right hand of the MapCanvas, displaying the different action to
 * to interact with Healpix sphere.
 * 
 * @author ejoliet
 * @version $Id: SimplePanel.java 121776 2010-01-30 18:28:11Z ejoliet $
 */
public class SimplePanel extends JPanel implements ItemListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** The canvas. */
	protected MapCanvas canvas;

	/** The colname. */
	public JComboBox colname;

	/** The nside. */
	JComboBox nside;

	// protected GaiaMapCanvas3D canvas;
	/** The nchoice. */
	protected Choice nchoice;

	/** The equat. */
	protected Checkbox nest, axis, all, equat;

	/** The colname sel. */
	protected String colnameSel;

	/** The jl title. */
	private JLabel jlTitle;

	/** The tooltip. */
	public JCheckBox tooltip;

	/** The grid. */
	private JCheckBox grid;

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
		jlTitle = new JLabel("Healpix Map");
		add(jlTitle);
		nside = new JComboBox();// new Choice();
		nside.setForeground(Color.blue);
		nside.addItem("1");
		nside.addItem("2");
		nside.addItem("4");
		nside.addItem("8");
		nside.addItem("16");
		nside.addItem("32");
		nside.addItem("64");
		nside.addItem("128");
		nside.setSelectedItem("64");
		nside.addItemListener(this);
		JPanel fac = new JPanel();
		fac.setLayout(new GridLayout(1, 2));
		JLabel delay = new JLabel("NSIDE:");
		delay.setForeground(Color.blue);
		fac.add(delay);
		fac.add(nside);
		// fac.setBackground(Color.white);
		JPanel grids = new JPanel(new GridLayout(7, 1));
		// grids.setBackground(Color.white);
		CheckboxGroup cbg = null;

		JPanel allp = new JPanel();
		all = new Checkbox("All data", cbg, true);
		all.setForeground(Color.blue);
		all.addItemListener(this);
		allp.add(all);

		JPanel np = new JPanel();
		nest = new Checkbox("Face", cbg, false);
		nest.setForeground(Color.blue);
		nest.addItemListener(this);
		nchoice = new Choice();
		nchoice.setForeground(Color.blue);

		for (int i = 0; i < 12; i++) {
			nchoice.addItem(Integer.toString(i));
		}

		nchoice.addItemListener(this);
		np.add(nest);
		np.add(nchoice);

		JPanel ap = new JPanel();
		axis = new Checkbox("Axis", cbg, true);
		axis.setForeground(Color.red);
		axis.addItemListener(this);
		ap.add(axis);
		JPanel gr = new JPanel();
		grid = new JCheckBox("Grid");
		grid.setSelected(true);
		grid.setForeground(Color.red);
		grid.addItemListener(this);
		gr.add(grid);
		JSlider sliderTransp = new JSlider(JSlider.HORIZONTAL,0,10,Math.round(canvas.getTransparency()*10));
		sliderTransp.setMajorTickSpacing(1);
		sliderTransp.setPaintLabels(true);
		sliderTransp.setPaintTicks(true);
		//sliderTransp.setMaximum(.getMaximumSize())
		sliderTransp.addChangeListener(new ChangeListener(){

			public void stateChanged(ChangeEvent e) {
				JSlider js = (JSlider) e.getSource();
				setAppearance(js.getValue()/10.0f);				
			}
			
		});
		gr.add(sliderTransp);
		JPanel ep = new JPanel();
		equat = new Checkbox("Equator", cbg, true);
		equat.setForeground(Color.red);
		equat.addItemListener(this);
		ep.add(equat);

		JPanel pfits = new JPanel();
		pfits.setLayout(new GridLayout(1, 2));
		colname = new JComboBox();
		colname.setForeground(Color.blue);
		colname.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				JComboBox jcb = (JComboBox) e.getSource();
				String tmp = (String) jcb.getSelectedItem();
				System.out.println("Selected=" + tmp);
				setSelectedCol(tmp);
			}
		});
		// colname.setAlignmentX(JComboBox.RIGHT_ALIGNMENT);
		JLabel jl1 = new JLabel("Data Map? ");
		jl1.setForeground(Color.blue);
		jl1.setHorizontalAlignment(JLabel.LEFT);
		pfits.add(jl1);
		pfits.add(colname);
		JPanel ptooltip = new JPanel();
		ptooltip.setLayout(new GridLayout(1, 1));
		tooltip = new JCheckBox("Tooltip enabled?");
		tooltip.setSelected(this.canvas.isToolTipEnabled());
		tooltip.setForeground(Color.black);
		tooltip.addItemListener(this);
		ptooltip.add(tooltip);
		grids.add(np);
		grids.add(allp);
		grids.add(ap);
		grids.add(gr);
		grids.add(ep);
		grids.add(pfits);
		grids.add(ptooltip);
		gridbag.setConstraints(grids, c);
		add(grids);
		gridbag.setConstraints(fac, c);
		add(fac);
		// setBackground(Color.white);
	}

	/**
	 * Sets the selected col.
	 * 
	 * @param tmp the new selected col
	 */
	protected void setSelectedCol(String tmp) {
		canvas.setColname(tmp);
		colnameSel = tmp;

	}
	
	/**
	 * Sets the appearance.
	 * 
	 * @param val the new appearance
	 */
	protected void setAppearance(float val) {
		//System.out.println("Selected val=" + val);
		canvas.setTransparency(val);
		canvas.updateFaces();

	}
	
	/**
	 * Gets the colname.
	 * 
	 * @return the colname
	 */
	public String getColname() {
		return colnameSel;
	}

	/** Set canvas */
	private void setCanvas(MapCanvas canvas) {
		this.canvas = canvas;
	}

	/**
	 * Instantiates a new simple panel.
	 */
	public SimplePanel(MapCanvas sky) {
		setCanvas(sky);
		init();
	}

	/** Item listener imp - for choice box */
	public void itemStateChanged(ItemEvent evt) {
		ItemSelectable is = evt.getItemSelectable();
		if (is == nside) {
			int ch = new Integer((String) nside.getSelectedItem()).intValue();
			canvas.setNside(ch);
			jlTitle.setText("Healpix Map");// (Current Nside = " + ch+")"
			return;
		}

		if (is == nchoice) {
			int ch = new Integer(nchoice.getSelectedItem()).intValue();
			canvas.setFace(ch);
			nest.setState(true);
			canvas.setViewNest(true);
			return;
		}
		if (is instanceof Checkbox) {
			Checkbox cb = (Checkbox) is;
			if (cb == all) {
				canvas.setViewAll(cb.getState());
				// unsetting face view
				// -> no point in displaying the face if all data are being
				// display
				nest.setState(false);
				canvas.setViewNest(nest.getState());
				return;
			}

			if (cb == axis) {
				canvas.setViewAxis(cb.getState());
				return;
			}
			if (cb == equat) {
				canvas.setViewEquator(cb.getState());
				return;
			}
			if (cb == nest) {
				canvas.setViewNest(cb.getState());
				// unsettin the all data view
				all.setState(false);
				canvas.setViewAll(all.getState());
				return;
			}
		} else {
			if (is instanceof JCheckBox) {
				if ((JCheckBox) is == tooltip) {
					canvas.setToolTip(((JCheckBox) is).isSelected());
					canvas.updateFaces();
					return;
				}
				if ((JCheckBox) is == grid) {
					canvas.setViewGrid(((JCheckBox) is).isSelected());
					return;
				}
			}
		}

	}

	/**
	 * Key pressed.
	 * 
	 * @param e the e
	 */
	public void keyPressed(KeyEvent e) {
	};

	/**
	 * Key released.
	 * 
	 * @param e the e
	 */
	public void keyReleased(KeyEvent e) {
	};

};
