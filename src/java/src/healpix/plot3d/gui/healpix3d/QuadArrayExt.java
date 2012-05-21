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

import healpix.essentials.Pointing;
import healpix.core.dm.HealpixMap;

import javax.media.j3d.QuadArray;

/**
 * Quadrilatere Array with tooltip fonctionality ready to be used by
 * {@link DataSphere}.
 *
 * @author ejoliet
 * @version $Id: QuadArrayExt.java 120812 2010-01-24 23:12:12Z ejoliet $
 */
public class QuadArrayExt extends QuadArray {

	/** The text. */
	String text;

	/** The ipix. */
	int ipix;

	/** The angle. */
	Pointing angle;

	/** The value. */
	double value;

	/** The index. */
	int index;

	/** The data. */
	double data[];
	/** The angular data */
	Pointing dataAngle[];
	/** The pixel ids */
	int dataIpix[];

	/**
	 * Instantiates a new quad array ext.
	 *
	 * @param nPoints the n points
	 * @param color the color
	 */
	QuadArrayExt(int nPoints, int color) {
		super(nPoints, color);
		data = new double[nPoints];
		dataAngle = new Pointing[nPoints];
		dataIpix = new int[nPoints];
		init();
	}
	/**
	 * Instantiates a new quad array ext.
	 *
	 * @param nPoints the n points
	 * @param color the color
	 */
	QuadArrayExt(HealpixMap ch, int nPoints, int color) {
		super(nPoints, color);
		data = new double[nPoints];
		dataAngle = new Pointing[nPoints];
		dataIpix = new int[nPoints];
		init();
	}
	/**
	 * Inits the.
	 */
	private void init() {
//		for (int u = 0; u < data.length; u++) {
//			data[u] = new HealpixDataIndex();
//		}
	}

	/**
	 * Sets the text.
	 *
	 * @param txt the new text
	 */
	public void setText(String txt) {
		this.text = txt;
	}

	/**
	 * Gets the text.
	 *
	 * @return the text
	 */
	public String getText() {
		return text;
	}

	/**
	 * Sets the ipix.
	 *
	 * @param ind the ind
	 * @param ipix the ipix
	 */
	public void setIpix(int ind, int ipix) {
		dataIpix[ind] = ipix;
	}

	/**
	 * Gets the ipix.
	 *
	 * @param ind the ind
	 *
	 * @return the ipix
	 */
	public int getIpix(int ind) {
		return dataIpix[ind];
	}

	/**
	 * Sets the ipix.
	 *
	 * @param ipix the new ipix
	 */
	public void setIpix(int ipix) {
		this.ipix = ipix;
	}

	/**
	 * Gets the ipix.
	 *
	 * @return the ipix
	 */
	public int getIpix() {
		return ipix;
	}

	/**
	 * Sets the angle.
	 *
	 * @param ind the ind
	 * @param ang the ang
	 */
	public void setAngle(int ind, Pointing ang) {
		dataAngle[ind] = ang;
	}

	/**
	 * Sets the angle.
	 *
	 * @param ang the new angle
	 */
	public void setAngle(Pointing ang) {
		this.angle = ang;
	}

	/**
	 * Gets the angle.
	 *
	 * @param ind the ind
	 *
	 * @return the angle
	 */
	public Pointing getAngle(int ind) {
		return dataAngle[ind];
	}

	/**
	 * Gets the angle.
	 *
	 * @return the angle
	 */
	public Pointing getAngle() {
		return angle;
	}

	/**
	 * Sets the value.
	 *
	 * @param ind the ind
	 * @param d the d
	 */
	public void setValue(int ind, double d) {
		data[ind] = d;
	}

	/**
	 * Sets the value.
	 *
	 * @param d the new value
	 */
	public void setValue(double d) {
		this.value = d;
	}

	/**
	 * Gets the tool tip txt.
	 *
	 * @param ind the ind
	 *
	 * @return the tool tip txt
	 */
	public String getToolTipTxt(int ind) {
		return "<html>" + "Value=" + data[ind] + "<br>"
				+ dataAngle[ind].toString() + "<br>" + "Healpix pixel:"
				+ dataIpix[ind] + "<html>";
	}

	/**
	 * Gets the tool tip txt.
	 *
	 * @return the tool tip txt
	 */
	public String getToolTipTxt() {
		return "<html>" + "Value=" + value + "<br>" + angle.toString() + "<br>"
				+ "Healpix pixel:" + ipix + "<html>";
	}
}
