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
package healpix.fits.test;

import junit.framework.TestCase;
import nom.tam.fits.BinaryTable;
import nom.tam.fits.BinaryTableHDU;
import nom.tam.fits.Fits;
import nom.tam.fits.FitsException;
import nom.tam.fits.Header;
import nom.tam.fits.TableHDU;

/**
 * Test class on fits reading
 * 
 * @author ejoliet
 * @version $Id: FitsTest.java 49444 2008-05-07 10:23:02Z ejoliet $
 */
public class FitsTest extends TestCase {
	/** Tables with the Chebyshev coefficients. */
	protected double[][][] tablePos;

	/** The table vel. */
	protected double[][][] tableVel;

	/** First allowed date in JD. */
	protected static double dateJDbeg = 2452641.;

	/** Last allowed date in JD. */
	protected static double dateJDend = 2458641.;

	/** Length in days of each Chebyshev expansion. */
	protected int step;

	/** The scheme fits. */
	private String schemeFits;

	/** The ncol. */
	private int ncol;

	/** The nrow. */
	private int nrow;

	/** The names. */
	private String[] names;

	/**
	 * Test reading binary table from fits
	 * 
	 * @param filename
	 *            fist file
	 * @return double[][] data read
	 * @throws Exception
	 */
	public double[][] readFitsBinaryTable(String filename) throws Exception {
		Fits fits = new Fits(filename);
		BinaryTableHDU thdu = (BinaryTableHDU) fits.getHDU(1);
		BinaryTable data = (BinaryTable) thdu.getData();
		Header head = thdu.getHeader();
		int ncard = head.getNumberOfCards();
		for (int n = 0; 0 < ncard; n++) {
			if (head.getKey(n).equals("ORDERING")) {
				schemeFits = head.getStringValue(head.getKey(n));
				System.out.println("Scheme found:" + schemeFits);
				break;
			}
		}
		ncol = data.getNCols();
		nrow = data.getNRows();
		System.out.println("Ncols, Nrows:" + ncol + ", " + nrow);
		char[] types = data.getTypes();
		double[][] array = new double[ncol][nrow];
		names = new String[ncol];
		for (int i = 0; i < ncol; i++) {
			if (types[i] != 'D')
				return null;
			double[][] tdoub = (double[][]) thdu.getColumn(i);
			names[i] = thdu.getColumnName(i);
			System.out.println("Name:" + names[i]);
			for (int j = 0; j < nrow; j++) {
				array[i][j] = tdoub[j][0];
			}
		}
		return array;
	}

	/**
	 * Test loading data from fits
	 * 
	 * @throws Exception
	 * @throws FitsException
	 */
	public void testLoadFitsData() throws Exception, FitsException {
		String testFITSFile = "data/test/test_2.ds";
		readFitsBinaryTable(testFITSFile);
		System.out.println(12 * Math.pow(2, 2));
		int nrows = (int) (12 * Math.pow(2, 2));
		assertEquals("Equals?", nrow, nrows);
	}

	/**
	 * Test reading the header table fits
	 * 
	 * @throws Exception
	 * @throws FitsException
	 */
	public void testReadTableHDU() throws Exception, FitsException {
		String testFITSFile = "data/test/FITSfile.fit";
		Fits fits = new Fits(testFITSFile);
		TableHDU table = (BinaryTableHDU) fits.getHDU(1);

		assertEquals(" Checks the first columns name : ", table
				.getColumnName(0), "Integer");
	}

}
