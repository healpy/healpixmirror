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
import healpix.core.HealpixIndex;
import healpix.core.base.set.LongRangeSet;
import healpix.tools.SpatialVector;


/** 
 * demonstrates high resolution
 * @deprecated
 */
public class HighResolution {

	public static void main(String[] args) throws Exception {
		HealpixIndex t = new HealpixIndex();
		
		/** 
		 * resolution 1 arc second  
		 */
		double res = HealpixIndex.getPixRes(262144);
		
		System.out.println("Minimum size of the pixel side is " + res
				+ " arcsec.");
		res = HealpixIndex.getPixRes(1048576);
		
		System.out.println("Minimum size of the pixel side is " + res
				+ " arcsec.");
		int nside = HealpixIndex.calculateNSide(1);
		System.out.println("NSIDE = "+nside);
		//>> NSIDE = 262144
		/**
		 * radius 20 degrees. Result set will have around  1e10 pixels!!
		 */
		double radius = Math.toRadians(20);

		
		/**
		 * random points on equator which is worst case scenario (poles are best case)
		 */		
		SpatialVector p1 =  HealpixIndex.ang2Vec(Math.toRadians(90), Math.toRadians(30));
		SpatialVector p2 =  HealpixIndex.ang2Vec(Math.toRadians(90), Math.toRadians(45));
		
		/**
		 * create disc around given pixels
		 */
		LongRangeSet disc1 = t.queryDisc( p1, radius, 0,1);
		LongRangeSet disc2 = t.queryDisc( p2, radius, 0,1);
		System.out.println("Number of pixels: "+disc1.size()); 
		System.out.println("Number of ranges: "+disc1.rangeCount());
		//>> Number of pixels: 24866171247
		//>> Number of ranges: 268977
		
		
		/**
		 * calculate intersection of two discs
		 */
		LongRangeSet intersect= disc1.intersect(disc2); 
		System.out.println("Intersect number of pixels: "+intersect.size()); 
		System.out.println("Intersect number of ranges: "+intersect.rangeCount());
		//>> Intersect number of pixels: 13374679095
		//>> Intersect number of ranges: 250765

		/*
		 * !!!! and all of this runs in 2 seconds and consumes only 20MB of memory !!!!
		 */
		
	}
}
