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
package healpix.core.base;

/**
 * This module provides functions that permute bits of the binary representation
 * of a number. Java bit manipulation class derived from Healpix fortran90
 * program.
 * 
 * @version $Id: BitManipulation.java 49444 2008-05-07 10:23:02Z ejoliet $
 * @author Benjamin D. Wandelt October 1997, edited by E. Hivon, October 2001 to
 *         be 'F' compatible
 */
public class BitManipulation {
	
	/** The magic1. */
	public static long magic1 = 89478485; // 101010101010101010101010101 LSB

	/** The magic2. */
	public static long magic2 = 178956970; // 1010101010101010101010101010 MSB

	/**
	 * default constructor
	 */
	public BitManipulation() {

	}

	/**
	 * swaps low and high bits in the word i
	 * 
	 * @param i
	 *            integer input word
	 * @return int a word with odd and even bits interchanged
	 */
	public long swapLSBMSB(long i) {
		long res = 0;
		long lsb = (i & magic1);
		long msb = (i & magic2);
		res = msb / 2 + lsb * 2;
		return res;
	}

	/**
	 * returns NOT i with even and odd bit positions interchanged
	 * 
	 * @param i
	 *            int input word
	 * @return int NOT (i with LSBMSB)
	 */
	public long invswapLSBMSB(long i) {
		long res = 0;
		long lsb = (i & magic1);
		long msb = (i & magic2);
		res = ~(msb / 2 + lsb * 2);
		return res;
	}

	/**
	 * returns i with even bits (0,2,4,...) inverted
	 * 
	 * @param i
	 *            int input word
	 * @return int word with modified bits
	 */
	public long invLSB(long i) {
		long res = 0;
		res = (i ^ magic1); // returns exclusive OR with odd bits
		return res;
	}

	/**
	 * returns i with odd bits (1,3,5,...) inverted
	 * 
	 * @param i
	 *            int input word
	 * @return int word with modified bits
	 */
	public long invMSB(long i) {
		long res = 0;
		res = (i ^ magic2);
		return res;
	}

	/**
	 * simulates behaviour of fortran90 MODULO function
	 * 
	 * @param a
	 *            double
	 * @param b
	 *            double
	 * @return double MODULO
	 */
	public double MODULO(double a, double b) {
		double res = 0.;
		long k = 0;
		if (a > 0.) {
			if (b > 0.) {
				k = (long) (a / b);
				res = a - k * b;
				return res;
			}
			if (b < 0.) {
				k = (long) Math.rint(a / b);
				res = a - k * b;
				return res;
			}
		}
		if (a <= 0.) {
			if (b <= 0.) {
				k = (long) (a / b);
				res = a - k * b;
				return res;
			}
			if (b > 0.) {
				k = (long) Math.rint(a / b);
				res = a - k * b;
				return res;
			}
		}
		return res;
	}
}
