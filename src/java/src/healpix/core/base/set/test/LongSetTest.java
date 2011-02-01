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

import healpix.core.base.set.LongIterator;
import healpix.core.base.set.LongSet;
import junit.framework.TestCase;

public class LongSetTest extends TestCase{
	
	public void testSet(){
		LongSet ls = new LongSet();
		
		assertTrue(ls.isEmpty());
		
		ls.add(100);
		ls.add(101);
		ls.add(102);
		ls.add(Integer.MAX_VALUE);
		
		assertEquals(ls.size(), 4);
		
		LongIterator iter = ls.longIterator();
		assertTrue(iter.hasNext());
		assertEquals(iter.next(), 100);
		assertTrue(iter.hasNext());
		assertEquals(iter.next(), 101);
		assertTrue(iter.hasNext());
		assertEquals(iter.next(), 102);
		assertTrue(iter.hasNext());
		assertEquals(iter.next(),Integer.MAX_VALUE);
		assertTrue(!iter.hasNext());


		
	}

}
