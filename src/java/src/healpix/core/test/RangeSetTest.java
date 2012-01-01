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

import healpix.core.RangeSet;
import junit.framework.TestCase;

public class RangeSetTest extends TestCase{

    public void testAddRange(){
        RangeSet b = new RangeSet();
        b.append(1, 11);
        b.append(30, 41);

        assertTrue(!b.contains(0));
        assertTrue(b.contains(1));
        assertTrue(b.contains(5));
        assertTrue(b.contains(10));
        assertTrue(!b.contains(11));
        assertTrue(!b.contains(29));
        assertTrue(b.contains(30));
        assertTrue(b.contains(35));
        assertTrue(b.contains(40));
        assertTrue(!b.contains(41));
    }
    public void testUnion(){
        RangeSet b1 = new RangeSet();
        RangeSet b2 = new RangeSet();
        RangeSet b3 = new RangeSet();
        RangeSet b4 = new RangeSet();
        b1.append(20, 31);
        b1.append(40, 51);

        b2.append(1,11);
        b2.append(45, 56);
        b4.setToUnion(b1,b2);

        b3.append(1,11);
        b3.append(20,31);
        b3.append(40,56);

        assertEquals(b3,b4);
    }

    public void testIntersect(){
        RangeSet b1 = new RangeSet();
        RangeSet b2 = new RangeSet();
        RangeSet b3 = new RangeSet();
        RangeSet b4 = new RangeSet();

        b1.append(20, 31);
        b1.append(40, 51);

        b2.append(1,11);
        b2.append(22,24);
        b2.append(45,56);
        b4.setToIntersection(b1,b2);

        b3.append(22,24);
        b3.append(45,51);
        assertEquals(b3,b4);
    }
    public void testIntersect2(){
        RangeSet b1 = new RangeSet();
        RangeSet b2 = new RangeSet();
        RangeSet b3 = new RangeSet();
        RangeSet b4 = new RangeSet();
        b1.append(10, 101);
        b1.append(110, 121);
        b1.append(200, 221);

        b2.append(20,31);
        b2.append(40,51);
        b2.append(90, 201);
        b4.setToIntersection(b1,b2);

        b3.append(20,31);
        b3.append(40,51);
        b3.append(90,101);
        b3.append(110,121);
        b3.append(200,201);

        assertEquals(b3,b4);
    }
    public void testSubstract(){
        RangeSet b1 = new RangeSet();
        RangeSet b2 = new RangeSet();
        RangeSet b3 = new RangeSet();
        RangeSet b4 = new RangeSet();
        b1.append(20, 31);
        b1.append(40, 51);

        b2.append(1,11);
        b2.append(45, 56);
        b4.setToDifference(b1,b2);

        b3.append(20,31);
        b3.append(40,45);

        assertEquals(b3,b4);
    }

    public void testContainsAll(){
        RangeSet b = new RangeSet();
        b.append(20, 31);
        b.append(40, 51);

        assertFalse(b.containsAll(0,11));
        assertFalse(b.containsAll(10,21));
        assertFalse(b.containsAll(19,20));
        assertTrue(b.containsAll(20,21));
        assertTrue(b.containsAll(21,22));
        assertTrue(b.containsAll(20,31));
        assertFalse(b.containsAll(25,36));
        assertTrue(b.containsAll(30,31));
        assertFalse(b.containsAll(31,32));
        assertFalse(b.containsAll(35,38));
        assertFalse(b.containsAll(35,46));
        assertTrue(b.containsAll(40,41));
        assertFalse(b.containsAll(45,56));
        assertFalse(b.containsAll(60,71));
    }
    public void testContainsAny(){
        RangeSet b = new RangeSet();
        b.append(20, 31);
        b.append(40, 51);

        assertFalse(b.containsAny(0,11));
        assertTrue(b.containsAny(10,21));
        assertFalse(b.containsAny(19,20));
        assertTrue(b.containsAny(20,21));
        assertTrue(b.containsAny(21,22));
        assertTrue(b.containsAny(20,31));
        assertTrue(b.containsAny(25,36));
        assertTrue(b.containsAny(30,37));
        assertFalse(b.containsAny(31,32));
        assertFalse(b.containsAny(35,38));
        assertTrue(b.containsAny(35,46));
        assertTrue(b.containsAny(40,41));
        assertTrue(b.containsAny(45,56));
        assertFalse(b.containsAny(60,71));
    }

}

