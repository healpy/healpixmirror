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
package healpix.core.test;

import healpix.core.Healpix;

import java.util.Date;

import junit.framework.TestCase;

/**
 * Test ring to nest scheme pixel index number
 * 
 * @deprecated
 * @author ejoliet
 * @version $Id: Ring2PixTest.java 44793 2008-03-06 16:48:56Z ejoliet $
 */
public class Ring2PixTest extends TestCase {

	/**
	 * Test ring to nest scheme pixel index number with nside = 2 and pixel
	 * number = 14
	 * 
	 * @throws Exception
	 */
	public void testRing2Pix14t2() throws Exception {
		int pix = Ring2Pix(2, 14);
		assertEquals("Wrong pix", 23, pix);

	}

	/**
	 * Test ring to nest scheme pixel index number
	 * 
	 * @param nside
	 *            resolution number
	 * @param pix
	 *            pixel index number in ring scheme
	 * @return pixel index number in nest scheme
	 * @throws Exception
	 */
	public int Ring2Pix(int nside, int pix) throws Exception {
		int npix = Healpix.ring2nest(nside, pix);
		System.out.println("Nest:" + npix + " = Ring:" + pix + " Nside is"
				+ nside);
		System.out.println("Finished  ::" + new Date());
		return npix;
	}

}
