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
package healpix.core.base.test;

import healpix.core.base.BitManipulation;
import junit.framework.TestCase;

/**
 * Test the {@link BitManipulation} class.
 * 
 * @author ejoliet
 * @version $Id: BitManipulationTest.java 47281 2008-04-07 15:20:46Z ejoliet $
 */
public class BitManipulationTest extends TestCase {
	/**
	 * Testing swap and invSwap
	 */
	public void testBitManipulation() {
		BitManipulation bm = new BitManipulation();
		long mag1 = BitManipulation.magic2;
		long a = 3;
		long b = bm.swapLSBMSB(a);
		assertEquals("swapLSBMSB=" + b, 1, a / b, 1e-10);
		a = 8;
		b = bm.swapLSBMSB(a);
		assertEquals("swapLSBMSB=" + b, 2, a / b, 1e-10);
		a = 3;
		b = bm.invswapLSBMSB(a);
		assertEquals("invswapLSBMSB=" + b, -4, b, 1e-10);
		a = 8;
		b = bm.invswapLSBMSB(a);
		assertEquals("invswapLSBMSB=" + b, -5, b, 1e-10);

		a = 3;
		b = bm.invMSB(a);
		assertEquals("invMSB=" + b, mag1 - 1, b, 1e-10);
		a = 8;
		b = bm.invMSB(a);
		assertEquals("invMSB=" + b, mag1 - 8, b, 1e-10);
	}

	/**
	 * Testing MODULO
	 */
	public void testMODULO() {
		BitManipulation bm = new BitManipulation();
		double a = 8.;
		double b = 5.;
		double mod = bm.MODULO(a, b);		
		System.out.println("a=" + a + " b=" + b + " mod=" + mod);
		a = -8.0;
		b = 5.0;
		mod = bm.MODULO(a, b);
		System.out.println("a=" + a + " b=" + b + " mod=" + mod);
		double mod1 = a%b;
		System.out.println("a=" + a + " b=" + b + " &=" + mod1);
		a = 8.0;
		b = -5.0;
		mod = bm.MODULO(a, b);
		System.out.println("a=" + a + " b=" + b + " mod=" + mod);
		a = -8.0;
		b = -5.0;
		mod = bm.MODULO(a, b);
		System.out.println("a=" + a + " b=" + b + " mod=" + mod);		
		a = 1.0;
		b = 4.0;
		mod = bm.MODULO(a, b);
		System.out.println("a=" + a + " b=" + b + " mod=" + mod);
	}
}
