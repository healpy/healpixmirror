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

package healpix.core.base.set.test;

import healpix.core.base.set.LongRangeIterator;
import healpix.core.base.set.LongRangeSetBuilder;
import junit.framework.TestCase;

public class LongRangeSetBuilderTest extends TestCase{
	
	public void testBuild(){
		LongRangeSetBuilder b = new LongRangeSetBuilder();
		
		b.appendRange(1,5);
		b.appendRange(10,15);
		b.append(16);
		b.appendRange(13,20);
		b.append(21);
		
		LongRangeIterator iter = b.build().rangeIterator();
		assertTrue(iter.moveToNext());
		assertEquals(1,iter.first());
		assertEquals(5,iter.last());

		assertTrue(iter.moveToNext());
		assertEquals(10,iter.first());
		assertEquals(21,iter.last());
		assertTrue(!iter.moveToNext());
	}

}

