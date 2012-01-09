/*
 * Experimental HEALPix Java code derived from the Gaia-developed Java sources
 * and the Healpix C++ library.
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

package healpix.newcore.test;

import junit.framework.TestCase;
import java.util.Random;
import java.text.DecimalFormat;

import healpix.newcore.*;

/** @author Martin Reinecke */
public class HealpixMapTest extends TestCase {

  public void test_swapScheme()throws Exception
    {
    System.out.println("\nTesting swapScheme()\n");

    for (int order=0; order<=8; ++order)
      {
      HealpixMapFloat map = new HealpixMapFloat (1L<<order,Scheme.RING);
      for (int i=0; i<map.getNpix(); ++i)
        map.setPixel(i,i);
      map.swapScheme();
      for (int i=0; i<map.getNpix(); ++i)
        assertEquals("inconsistency",map.nest2ring(i),(int)map.getPixel(i));
      map.swapScheme();
      for (int i=0; i<map.getNpix(); ++i)
        assertEquals("inconsistency",i,(int)map.getPixel(i));
      }
    }
  }
