/*
 *  This file is part of Healpix Java.
 *
 *  This code is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This code is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this code; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  For more information about HEALPix, see http://healpix.sourceforge.net
 */
package healpix.essentials.test;

import healpix.essentials.RangeSet;
import junit.framework.TestCase;
import java.io.*;

public class RangeSetTest extends TestCase {

  public void testAppend()
    {
    RangeSet b = new RangeSet();
    b.append(1,11);
    assertEquals(b,new RangeSet(new long[]{1,11}));
    b.append(10,15);
    assertEquals(b,new RangeSet(new long[]{1,15}));
    b.append(1,15);
    assertEquals(b,new RangeSet(new long[]{1,15}));
    b.append(7,15);
    assertEquals(b,new RangeSet(new long[]{1,15}));
    b.append(30,41);
    assertEquals(b,new RangeSet(new long[]{1,15,30,41}));
    try
      {
      b.append(29,31);
      fail("Should have raised an IllegalArgumentException");
      }
    catch (IllegalArgumentException expected) {}
    }
  public void testContains()
    {
    RangeSet b=new RangeSet(new long[]{1,11,30,41});
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
  public void testAdd()
    {
    RangeSet b = new RangeSet();
    b.add(5, 11);
    assertEquals(b,new RangeSet(new long[]{5,11}));
    b.add(1, 7);
    assertEquals(b,new RangeSet(new long[]{1,11}));
    b.add(1, 11);
    assertEquals(b,new RangeSet(new long[]{1,11}));
    b.add(30, 41);
    assertEquals(b,new RangeSet(new long[]{1,11,30,41}));
    b.add(1, 11);
    assertEquals(b,new RangeSet(new long[]{1,11,30,41}));
    b.add(-1,0);
    assertEquals(b,new RangeSet(new long[]{-1,0,1,11,30,41}));
    b.add(-2,-1);
    assertEquals(b,new RangeSet(new long[]{-2,0,1,11,30,41}));
    b.add(-2,-1);
    assertEquals(b,new RangeSet(new long[]{-2,0,1,11,30,41}));
    b.add(2, 11);
    assertEquals(b,new RangeSet(new long[]{-2,0,1,11,30,41}));
    b.add(1, 10);
    assertEquals(b,new RangeSet(new long[]{-2,0,1,11,30,41}));
    b.add(15, 21);
    assertEquals(b,new RangeSet(new long[]{-2,0,1,11,15,21,30,41}));
    }
  public void testRemove()
    {
    RangeSet b = new RangeSet(new long[]{0,11,20,31});
    b.remove(5,25);
    assertEquals(b,new RangeSet(new long[]{0,5,25,31}));
    b.remove(31,32);
    assertEquals(b,new RangeSet(new long[]{0,5,25,31}));
    b.remove(35,38);
    assertEquals(b,new RangeSet(new long[]{0,5,25,31}));
    b.remove(-90,-80);
    assertEquals(b,new RangeSet(new long[]{0,5,25,31}));
    b.remove(27,29);
    assertEquals(b,new RangeSet(new long[]{0,5,25,27,29,31}));
    b.remove(25,26);
    assertEquals(b,new RangeSet(new long[]{0,5,26,27,29,31}));
    b.remove(4,6);
    assertEquals(b,new RangeSet(new long[]{0,4,26,27,29,31}));
    b.remove(-20,40);
    assertEquals(b,new RangeSet(new long[]{}));
    b.remove(-20,40);
    assertEquals(b,new RangeSet(new long[]{}));
    }
  public void testIntersect2()
    {
    RangeSet b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(2,29);
    assertEquals(b,new RangeSet(new long[]{2,11,20,29}));
    b.intersect(-8,50);
    assertEquals(b,new RangeSet(new long[]{2,11,20,29}));
    b.intersect(2,50);
    assertEquals(b,new RangeSet(new long[]{2,11,20,29}));
    b.intersect(2,29);
    assertEquals(b,new RangeSet(new long[]{2,11,20,29}));
    b.intersect(-18,29);
    assertEquals(b,new RangeSet(new long[]{2,11,20,29}));
    b.intersect(3,11);
    assertEquals(b,new RangeSet(new long[]{3,11}));
    b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(3,15);
    assertEquals(b,new RangeSet(new long[]{3,11}));
    b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(17,30);
    assertEquals(b,new RangeSet(new long[]{20,30}));
    b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(11,20);
    assertEquals(b,new RangeSet(new long[]{}));
    b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(-8,-7);
    assertEquals(b,new RangeSet(new long[]{}));
    b = new RangeSet(new long[]{0,11,20,31});
    b.intersect(31,35);
    assertEquals(b,new RangeSet(new long[]{}));
    }
  public void testUnion()
    {
    assertEquals(new RangeSet(new long[]{1,11,20,31,40,56}),
                 new RangeSet(new long[]{20,31,40,51}).union
                 (new RangeSet(new long[]{1,11,45,56})));
    assertEquals(new RangeSet(new long[]{1,11,45,56}),
                 new RangeSet(new long[]{}).union
                 (new RangeSet(new long[]{1,11,45,56})));
    assertEquals(new RangeSet(new long[]{1,11,45,56}),
                 new RangeSet(new long[]{1,11,45,56}).union
                 (new RangeSet(new long[]{})));
    }

  public void testIntersect()
    {
    assertEquals(new RangeSet(new long[]{22,24,45,51}),
                 new RangeSet(new long[]{20,31,40,51}).intersection
                 (new RangeSet(new long[]{1,11,22,24,45,56})));
    assertEquals(new RangeSet(new long[]{20,31,40,51,90,101,110,121,200,201}),
                 new RangeSet(new long[]{10,101,110,121,200,221}).intersection
                 (new RangeSet(new long[]{20,31,40,51,90,201})));
    assertEquals(new RangeSet(new long[]{}),
                 new RangeSet(new long[]{20,31,40,51}).intersection
                 (new RangeSet(new long[]{})));
    assertEquals(new RangeSet(new long[]{}),
                 new RangeSet(new long[]{}).intersection
                 (new RangeSet(new long[]{20,31,40,51})));
    }
  public void testSubstract()
    {
    assertEquals(new RangeSet(new long[]{20,31,40,45}),
                 new RangeSet(new long[]{20,31,40,51}).difference
                 (new RangeSet(new long[]{1,11,45,56})));
    assertEquals(new RangeSet(new long[]{}),
                 new RangeSet(new long[]{}).difference
                 (new RangeSet(new long[]{1,11,45,56})));
    assertEquals(new RangeSet(new long[]{1,11,45,56}),
                 new RangeSet(new long[]{1,11,45,56}).difference
                 (new RangeSet(new long[]{})));
    }

  public void testContainsAll()
    {
    RangeSet b = new RangeSet(new long[]{20,31,40,51});

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
  public void testContainsAll2()
    {
    RangeSet b = new RangeSet(new long[]{20,31,40,51});

    assertTrue(b.containsAll(new RangeSet(new long[]{20,31,40,51})));
    assertTrue(b.containsAll(new RangeSet(new long[]{20,21})));
    assertTrue(b.containsAll(new RangeSet(new long[]{50,51})));
    assertFalse(b.containsAll(new RangeSet(new long[]{19,31,40,51})));
    assertFalse(b.containsAll(new RangeSet(new long[]{20,31,40,52})));
    assertFalse(b.containsAll(new RangeSet(new long[]{20,51})));
    assertFalse(b.containsAll(new RangeSet(new long[]{0,1})));
    assertFalse(b.containsAll(new RangeSet(new long[]{0,20,31,40,51,100})));
    }
  public void testContainsAny()
    {
    RangeSet b = new RangeSet(new long[]{20,31,40,51});

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
  public void testContainsAny2()
    {
    RangeSet b = new RangeSet(new long[]{20,31,40,51});

    assertTrue(b.containsAny(new RangeSet(new long[]{20,31,40,51})));
    assertTrue(b.containsAny(new RangeSet(new long[]{20,21})));
    assertTrue(b.containsAny(new RangeSet(new long[]{50,51})));
    assertTrue(b.containsAny(new RangeSet(new long[]{19,31,40,51})));
    assertTrue(b.containsAny(new RangeSet(new long[]{20,31,40,52})));
    assertTrue(b.containsAny(new RangeSet(new long[]{20,51})));
    assertFalse(b.containsAny(new RangeSet(new long[]{0,1})));
    assertFalse(b.containsAny(new RangeSet(new long[]{0,20,31,40,51,100})));
    }

  public void testIterator()
    {
    RangeSet b = new RangeSet(new long[]{20,31,40,51});
    RangeSet.ValueIterator it =b.valueIterator();
    for (int i=0; i<b.size(); ++i)
      for (long j=b.ivbegin(i); j<b.ivend(i); ++j)
        {
        assertTrue(it.hasNext());
        assertEquals("value mismatch", j, it.next());
        }
    assertFalse(it.hasNext());
    }

  public void testSerialize() throws IOException, ClassNotFoundException {
      RangeSet r = new RangeSet(new long[]{10,20,30,40,50,51});
      ByteArrayOutputStream s = new ByteArrayOutputStream();
      new ObjectOutputStream(s).writeObject(r);

      RangeSet r2 = (RangeSet) new ObjectInputStream(new ByteArrayInputStream(s.toByteArray())).readObject();

      assertEquals(r,r2);
  }
}
