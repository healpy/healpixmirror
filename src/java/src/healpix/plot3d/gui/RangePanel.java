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

import java.awt.Color;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;

/**
 * Set the range for the map to be displayed
 * 
 * @author ejoliet
 * @version $Id: RangePanel.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class RangePanel extends Panel implements TextListener {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The low. */
	protected TextField low;

	/** The high. */
	protected TextField high;

	/**
	 * Name and attrib will probably be the same but may differ slightly
	 */
	public RangePanel(String name, String get, String attrib, String cls) {
		Label title = new Label(name, Label.CENTER);
		title.setForeground(Color.blue);
		low = new TextField("     0", 5);
		Label lowl = new Label(">");
		lowl.setForeground(Color.blue);
		high = new TextField(" 1000", 5);
		Label highl = new Label(" and <");
		highl.setForeground(Color.blue);
		// this.setLayout(new GridLayout(2,1,0,0));
		add(title);
		add(lowl);
		add(low);
		add(highl);
		add(high);
		low.addTextListener(this);
		high.addTextListener(this);
		critInit(get, attrib, cls);
	}

	/**
	 * Crit init.
	 * 
	 * @param get the get
	 * @param attrib the attrib
	 * @param cls the cls
	 */
	protected void critInit(String get, String attrib, String cls) {
	}

	/* (non-Javadoc)
	 * @see java.awt.event.TextListener#textValueChanged(java.awt.event.TextEvent)
	 */
	public void textValueChanged(TextEvent e) {
	}
}
