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
import junit.framework.TestCase;

//test on high resolution
/** @deprecated */
public class HiResTest extends TestCase{
	final int NSIDE = 4096;//262144;//1048576; //highest res available with long ranges
	final double D2R = Math.PI/180d;
	final SpatialVector V = new SpatialVector(1, 1, 1);

	/**
	 * test on high resolutions. If range set are working correctly, no OutOfMemory is generated
	 * 
	 */
	public void testQueryRing() throws Exception{
		HealpixIndex ps = new HealpixIndex(NSIDE);
		V.normalized();
		LongRangeSet rs = null;
		System.out.println("ring 1d");
		rs = ps.queryDisc( V, D2R * 1, 1,0);
		System.out.println(rs.size());
		
		System.out.println("ring 10d");
		rs = ps.queryDisc(V, D2R * 10, 1,0);
		System.out.println(rs.size());
		
		
	}

}
