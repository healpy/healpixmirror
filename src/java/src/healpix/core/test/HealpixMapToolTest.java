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

import healpix.core.dm.HealpixMap;
import healpix.core.dm.util.HealpixTool;
import healpix.tools.HealpixMapCreator;
import junit.framework.TestCase;

/**
 * The Class HealpixMapToolTest.
 */
public class HealpixMapToolTest extends TestCase {
	
	/**
	 * Test degrade.
	 */
	public void testDegrade() {
		int nside_in = 4;
		int nside_out = 2;
		HealpixMapCreator cr = new HealpixMapCreator(nside_in);//nside=2,48 pixels, 4 per faces (12)
		HealpixMap map = cr.getMap();
		//Set Particular value:
		map.setValueCell(0, 1);
		HealpixTool tool = new HealpixTool(cr.getMap());
	
		try {
//			Same max?
			System.out.println("Map max:"+map.getMax());
			HealpixMap map_down = tool.degrade(nside_out); //12 pixels -> pixel val=faces(if nside=2) 	
			System.out.println("Map degraded max:"+map_down.getMax());
			assertEquals("Disagree on Max",map.getMax(),(cr.getMap().nPixel()/map_down.nPixel())*map_down.getMax());
			
			//Same ipix value for the position (1,1)?
			assertEquals("Disagree on ipix pos",map.get(0, 0),(cr.getMap().nPixel()/map_down.nPixel())*map_down.get(0, 0));
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Test upgrade.
	 */
	public void testUpgrade() {
		HealpixMapCreator cr = new HealpixMapCreator(1);
		HealpixMap map = cr.getMap();
		//Set Particular value:
		map.setValueCell(0, 1);
		HealpixTool tool = new HealpixTool(cr.getMap());
	
		try {
//			Same max?
			System.out.println("Map max:"+map.getMax());
			HealpixMap map_up = tool.upgrade(2);
			System.out.println("Map upgraded max:"+map_up.getMax());
			assertEquals("Disagree on Max",map.getMax(),map_up.getMax());
			
			//Same ipix value for the position (1,1)?
			assertEquals("Disagree on ipix pos",map.get(0, 0),map_up.get(0, 0));
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
}