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
package healpix.fits.test;
import healpix.fits.*;
import healpix.core.*;

import junit.framework.TestCase;

public class FitsUtilTest extends TestCase
  {

  public void testHPmapf() throws Exception
    {
    HealpixMapFloat hpmf = FitsUtil.getFloatMap("/scratch/martin/sim/output/sky.fits",2,1);
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
    hpmf.swapScheme();
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
    hpmf.swapScheme();
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
//    FitsUtil.writeFloatMap(hpmf,"!blub.fits");
    }
  public void testHPmapd() throws Exception
    {
    HealpixMapDouble hpmf = FitsUtil.getDoubleMap("/scratch/martin/sim/output/sky.fits",2,1);
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
    hpmf.swapScheme();
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
    hpmf.swapScheme();
    System.out.println(hpmf.getPixel(0)+" " + hpmf.getPixel(1)+" "+hpmf.getPixel(hpmf.getNpix()-1));
    }
  }
