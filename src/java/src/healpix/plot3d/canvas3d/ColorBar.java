/*
 * HEALPix Java code supported by the Gaia project.
 *
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
package healpix.plot3d.canvas3d;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.text.DecimalFormat;
import java.text.Format;

import javax.swing.JComponent;

/**
 * Displaying a bar of colors (the scale) using a ColorTransform and the min and
 * max values.
 * 
 * @author ejoliet
 * @version $Id: ColorBar.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class ColorBar extends JComponent {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;
	
	/** The formatter. */
	Format formatter = new DecimalFormat("0.000E0");
	
	/** The ctrans. */
	ColorTransform ctrans;

	/** The scale color. */
	double scaleColor;

	/**
	 * Default constructor. Uses the {@link SineColorTransform}.
	 */
	public ColorBar() {
		setMinimumSize(new Dimension(10, 20));
		ctrans = new SineColorTransform(0, 1); // There is only one right now!
		// Hope more to come!
	}

	/**
	 * Creates a new ColorBar object.
	 * 
	 * @param ctrans
	 *            a color transformation
	 */
	public ColorBar(ColorTransform ctrans) {
		this.ctrans = ctrans;
		scaleColor = ctrans.getScaleColor();
		setMinimumSize(new Dimension(10, 20));
	}

	/* (non-Javadoc)
	 * @see javax.swing.JComponent#getPreferredSize()
	 */
	public Dimension getPreferredSize() {
		return new Dimension(10, 40);
	}

	/* (non-Javadoc)
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	public void paintComponent(Graphics g) {
		try {
			super.paintComponent(g);

			Graphics2D g2D = (Graphics2D) g;

			int width = this.getWidth();

			int numBoxes = ((width - 1) / 11);

			g2D.setColor(Color.BLACK);
			g2D.drawString("Min:" + formatter.format(ctrans.getMin()), 0, 40);

			for (int i = 0; i < numBoxes; i++) {
				double valTmp = ctrans.getMin()
						+ ((ctrans.getScaleColor() * i) / numBoxes);
				g2D.setColor(ctrans.getColor(valTmp, ctrans.getMin(), ctrans
						.getMax()));
				g2D.fillRect((i * 11) + 1, 1, 11, 18);
			}

			String message = "Max:" + formatter.format(ctrans.getMax());
			g2D.setColor(Color.BLACK);
			g2D.drawString(message, width - message.length() - 90, 40);
		} catch (Exception e) {
			// TODO do proper exception handling
		}
	}

	/**
	 * Gets the contrast.
	 * 
	 * @param color the color
	 * 
	 * @return the contrast
	 */
	public static Color getContrast(Color color) {
		double r = ((double) color.getRed() / 255);
		double g = ((double) color.getGreen() / 255);
		double b = ((double) color.getBlue() / 255);

		double luminance = (0.25 * r) + (0.625 * g) + (0.125 * b);

		if (luminance > 0.5) {
			return Color.black;
		} else {
			return Color.white;
		}
	}

	/**
	 * Update.
	 * 
	 * @param ct the ct
	 */
	public void update(ColorTransform ct) {
		this.ctrans = ct;
		repaint();
	}
}
