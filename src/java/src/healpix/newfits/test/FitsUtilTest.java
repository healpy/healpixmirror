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
import healpix.newcore.*;

import junit.framework.TestCase;
import java.util.UUID;
import java.io.File;

public class FitsUtilTest extends TestCase
  {
  public void testHPmapf() throws Exception
    {
    String name = UUID.randomUUID().toString()+".fits";
    HealpixMapFloat hpmf = new HealpixMapFloat(128,Scheme.NESTED);
    for (long i=0; i<hpmf.getNpix(); ++i)
      hpmf.setPixel(i,(float)i);
    FitsUtil.writeFloatMap(hpmf,name);
    hpmf.setNsideAndScheme (64, Scheme.RING);
    hpmf=FitsUtil.getFloatMap(name,2,1);
    new File(name).delete();
    assertEquals("Scheme problem",Scheme.NESTED,hpmf.getScheme());
    assertEquals("Nside problem",128,hpmf.getNside());
    for (long i=0; i<hpmf.getNpix(); ++i)
      assertEquals("Value problem",(float)i,hpmf.getPixel(i));
    }
  }
