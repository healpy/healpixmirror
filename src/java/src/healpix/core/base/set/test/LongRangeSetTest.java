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
import healpix.core.base.set.LongRangeSet;
import healpix.core.base.set.LongRangeSetBuilder;
import healpix.core.base.set.LongSet;
import junit.framework.TestCase;

public class LongRangeSetTest extends TestCase {

    LongRangeSetBuilder b = new LongRangeSetBuilder();
    LongRangeSetBuilder b2 = new LongRangeSetBuilder();
    LongRangeSetBuilder b3 = new LongRangeSetBuilder();

    public void testAddRange(){
        b.appendRange(1, 10);
        b.appendRange(30, 40);

        LongRangeSet rs = b.build();

        assert(!rs.contains(0));
        assert(rs.contains(1));
        assert(rs.contains(5));
        assert(rs.contains(10));
        assert(!rs.contains(11));
        assert(!rs.contains(29));
        assert(rs.contains(30));
        assert(rs.contains(35));
        assert(rs.contains(40));
        assert(!rs.contains(41));
    }

    public void testIter(){
        b.appendRange(1, 10);
        b.appendRange(30, 40);

        LongRangeSet rs2 = b.build();
        LongSet rs = new LongSet(rs2.longIterator());

        assert(!rs.contains(0));
        assert(rs.contains(1));
        assert(rs.contains(5));
        assert(rs.contains(10));
        assert(!rs.contains(11));
        assert(!rs.contains(29));
        assert(rs.contains(30));
        assert(rs.contains(35));
        assert(rs.contains(40));
        assert(!rs.contains(41));
    }
    
    public void testComplement(){
    	b.appendRange(20,30);
    	b.append(40);
    	b.append(42);
    	b.appendRange(50,60);
    	LongRangeSet rs = b.build();        	
    	
    	LongRangeIterator iter = rs.complement().rangeIterator();
    	assertTrue(iter.moveToNext());
    	assertEquals(Long.MIN_VALUE, iter.first());
    	assertEquals(19, iter.last());
    	assertTrue(iter.moveToNext());
    	assertEquals(31, iter.first());
    	assertEquals(39, iter.last());
    	assertTrue(iter.moveToNext());
    	assertEquals(41, iter.first());
    	assertEquals(41, iter.last());
    	assertTrue(iter.moveToNext());
    	assertEquals(43, iter.first());
    	assertEquals(49, iter.last());
    	assertTrue(iter.moveToNext());
    	assertEquals(61, iter.first());
    	assertEquals(Long.MAX_VALUE, iter.last());
    	assertTrue(!iter.moveToNext());

    	
    	assertEquals(rs.complement().complement(), rs);
    }
    
    public void testUnion(){
    	b.appendRange(20, 30);
    	b.appendRange(40, 50);
    	LongRangeSet r1 = b.build();
    	
    	b2.appendRange(1,10);
    	b2.appendRange(45, 55);
    	LongRangeSet r2 = b2.build();
    	
    	b3.appendRange(1,10);
    	b3.appendRange(20,30);
    	b3.appendRange(40,55);
    	LongRangeSet r3 = b3.build();
    	
    	assertEquals(r3,r1.union(r2));
    }
    
    public void testIntersect(){
    	b.appendRange(20, 30);
    	b.appendRange(40, 50);
    	LongRangeSet r1 = b.build();
    	
    	b2.appendRange(1,10);
    	b2.appendRange(22,23);
    	b2.appendRange(45, 55);
    	LongRangeSet r2 = b2.build();
    	
    	b3.appendRange(22,23);
    	b3.appendRange(45,50);
    	LongRangeSet r3 = b3.build();
    	
    	assertEquals(r3,r1.intersect(r2));
    }
    
    public void testIntersect2(){
    	b.appendRange(10, 100);
    	b.appendRange(110, 120);
    	b.appendRange(200, 220);
    	LongRangeSet r1 = b.build();
    	
    	b2.appendRange(20,30);
    	b2.appendRange(40,50);
    	b2.appendRange(90, 200);
    	LongRangeSet r2 = b2.build();
    	
    	b3.appendRange(20,30);
    	b3.appendRange(40,50);
    	b3.appendRange(90,100);
    	b3.appendRange(110,120);
    	b3.appendRange(200,200);
    	LongRangeSet r3 = b3.build();
    	
    	assertEquals(r3,r1.intersect(r2));
    }
    
    public void testSubstract(){
    	b.appendRange(20, 30);
    	b.appendRange(40, 50);
    	LongRangeSet r1 = b.build();
    	
    	b2.appendRange(1,10);
    	b2.appendRange(45, 55);
    	LongRangeSet r2 = b2.build();
    	
    	b3.appendRange(20,30);
    	b3.appendRange(40,44);
    	LongRangeSet r3 = b3.build();    	    	
    	
    	assertEquals(r3,r1.substract(r2));
    }
    
    public void testContainsAll(){
    	b.appendRange(20, 30);
    	b.appendRange(40, 50);
    	LongRangeSet r1 = b.build();
    	
    	assertFalse(r1.containsAll(0,10));
    	assertFalse(r1.containsAll(10,20));
    	assertFalse(r1.containsAll(19,19));
    	assertTrue(r1.containsAll(20,20));
    	assertTrue(r1.containsAll(21,21));
    	assertTrue(r1.containsAll(20,30));
    	assertFalse(r1.containsAll(25,35));
    	assertTrue(r1.containsAll(30,30));
    	assertFalse(r1.containsAll(31,31));
    	assertFalse(r1.containsAll(35,37));
    	assertFalse(r1.containsAll(35,45));;
    	assertTrue(r1.containsAll(40,40));
    	assertFalse(r1.containsAll(45,55));
    	assertFalse(r1.containsAll(60,70));    	
    }


    public void testContainsAny(){
    	b.appendRange(20, 30);
    	b.appendRange(40, 50);
    	LongRangeSet r1 = b.build();
    	
    	assertFalse(r1.containsAny(0,10));
    	assertTrue(r1.containsAny(10,20));
    	assertFalse(r1.containsAny(19,19));
    	assertTrue(r1.containsAny(20,20));
    	assertTrue(r1.containsAny(21,21));
    	assertTrue(r1.containsAny(20,30));
    	assertTrue(r1.containsAny(25,35));    	
    	assertTrue(r1.containsAny(30,30));
    	assertFalse(r1.containsAny(31,31));
    	assertFalse(r1.containsAny(35,37));
    	assertTrue(r1.containsAny(35,45));;
    	assertTrue(r1.containsAny(40,40));
    	assertTrue(r1.containsAny(45,55));
    	assertFalse(r1.containsAny(60,70));    	
    }

}
