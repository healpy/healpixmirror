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

import healpix.plot3d.canvas3d.ToolTipBehavior;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Point;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.border.EmptyBorder;

/**
 * Construct the tooltip dialog window for the {@link ToolTipBehavior}.
 * 
 * @author ejoliet
 * @version $Id: DialogToolTip.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class DialogToolTip extends JDialog {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The tip. */
	static DialogToolTip tip = new DialogToolTip();

	/** The label. */
	static JLabel label = new JLabel("", JLabel.CENTER);
	static {
		tip.setUndecorated(true);
		tip.getContentPane().setBackground(Color.yellow); // or whatever it is
		tip.getContentPane().setLayout(new BorderLayout());
		tip.getContentPane().add(label);

		tip.setModal(false);

		label.setBorder(new EmptyBorder(2, 2, 2, 2));
		label.setBackground(Color.yellow); // just in case
	}

	/**
	 * Shows the text htmlText on a tooltip at a particular position.
	 * 
	 * @param locationOnScreen
	 *            the position of the tooltip dialog window
	 * @param htmlText
	 *            text to show
	 */
	public static void showToolTip(Point locationOnScreen, String htmlText) {
		if (htmlText == null)
			tip.setVisible(false);
		else {
			label.setText(htmlText);
			tip.pack(); // resize around the new label

			// on Macs, sometimes the initial pack gets the wrong Y value --
			// pack again
			tip.pack(); // resize around the new label
			tip.setLocation(locationOnScreen);
			tip.setVisible(true);
		}
	}

	/**
	 * Hide the window tooltip.
	 */
	public static void hideToolTip() {
		tip.setVisible(false);
	}
}
