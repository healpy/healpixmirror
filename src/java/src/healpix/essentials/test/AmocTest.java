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

import healpix.essentials.Moc;
import healpix.essentials.MocUtil;
import healpix.essentials.RangeSet;
import junit.framework.TestCase;
import java.io.*;

public class AmocTest extends TestCase
  {
  public void testSimple() throws Exception
    {
    Moc moc=new Moc();
    moc.addPixelRange(0,4,5);
    moc.addPixelRange(0,6,7);
    moc.addPixelRange(2,4,17);
    moc.addPixelRange(10,3000000,3000001);

    assertEquals("inconsistency",moc,moc.complement().complement());
    assertEquals("inconsistency",moc,MocUtil.mocFromString("0/4, 6 2/ 4 -16 10/3000000"));
    assertEquals("inconsistency",moc,MocUtil.mocFromString("0/6 2/ 5 2/4 2/6- 16 0/4  10/3000000"));
    assertEquals("inconsistency",moc,MocUtil.mocFromString
      ("{\"0\":[6] , \"2\": [5 ], \"2\":[  4,6,7,8,9,10,11,12,13,14,15,16], \"0\":[4],  \"10\":[3000000]}"));
    assertEquals("inconcistency",moc,MocUtil.mocFromString(MocUtil.mocToStringASCII(moc)));
    assertEquals("inconcistency",moc,MocUtil.mocFromString(MocUtil.mocToStringJSON(moc)));
    assertEquals("inconsistency",moc,Moc.fromUniq(moc.toUniq()));
    assertEquals("inconcistency",moc.maxOrder(),10);
    Moc xtmp = moc.degradedToOrder(8,false);
    assertTrue("inconsistency",moc.contains(xtmp));
    assertFalse("inconsistency",xtmp.contains(moc));
    assertTrue("inconsistency",xtmp.overlaps(moc));
    xtmp=moc.degradedToOrder(8,true);
    assertFalse("inconsistency",moc.contains(xtmp));
    assertTrue("inconsistency",xtmp.contains(xtmp));
    assertEquals("inconsistency",moc,Moc.fromCompressed(moc.toCompressed()));
    ByteArrayOutputStream out= new ByteArrayOutputStream();
    MocUtil.mocToFits(moc,out);
    ByteArrayInputStream inp = new ByteArrayInputStream(out.toByteArray());
    assertEquals("inconsistency",moc,MocUtil.mocFromFits(inp));
    }
  }
