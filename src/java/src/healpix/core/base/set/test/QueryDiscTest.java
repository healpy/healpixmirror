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

import healpix.core.HealpixIndex;
import healpix.core.base.set.LongList;
import healpix.core.base.set.LongSet;
import healpix.core.dm.HealpixMap;
import healpix.tools.HealpixMapCreator;
import healpix.tools.SpatialVector;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;


public class QueryDiscTest extends TestCase {
	public void testQueryDisc () throws Exception {
	int nside = 32;
	
	boolean inclusive = false;
	double radius = Math.PI;
	double radius1 = Math.PI/2.;
    HealpixIndex pt = new HealpixIndex(nside);
    long npix = HealpixIndex.nside2Npix(nside);
    double res = HealpixIndex.getPixRes(nside); // pixel size in radians
    double pixSize = Math.toRadians(res/3600.0); // pixel size in radians
    System.out.println("res="+res+"  pixSize="+pixSize+" rad");

    SpatialVector vec =  new SpatialVector(0., 0., 1.);
    double[] ang = HealpixIndex.vec2Ang(vec);
    System.out.println(" Vector is (ra,dec)("+vec.ra()+", "+vec.dec()+") and angle is (theta,phi)("+ang[0]+","+ang[1]+")");

    SpatialVector dvec =  new SpatialVector(0., 0., -1.);
    double[] dang = HealpixIndex.vec2Ang(dvec);
    System.out.println(" Vector is (ra,dec)("+dvec.ra()+", "+dvec.dec()+") and angle is (theta,phi)("+dang[0]+","+dang[1]+")");

    LongList fullSky = new LongList(pt.queryDisc(vec, radius,  inclusive));
    assertEquals(npix, fullSky.size());

    LongList firstHalfSky = new LongList(pt.queryDisc( vec, radius1+1e-12, inclusive));
//    dump(firstHalfSky);

    // you get exactly nside*2 more pixels from the equator which is included in north
    assertEquals(npix/2 + (nside*2), firstHalfSky.size());
    LongList secondHalfSky = new LongList(pt.queryDisc( dvec, radius1-1e-12, inclusive));
   // dump(secondHalfSky);
    System.out.println();

    // those included north are not included south    
    assertEquals(npix/2 - (nside*2), secondHalfSky.size());
	HealpixMapCreator cre = new HealpixMapCreator(nside);
	HealpixMap map = cre.getMap();
	List<Long> dups = new ArrayList<Long>();

    for (int p=0; p < secondHalfSky.size(); p++) {
		map.add((int)secondHalfSky.get(p), (double)(1+p*10));

		if (firstHalfSky.contains(secondHalfSky.get(p))){
			dups.add(secondHalfSky.get(p));
		}
    }


   /**	    
    MapView3d mview = new MapView3d(false);
	mview.setMap(map);
	mview.setSize(800, 800);
	mview.setVisible(true);
	//mview.wait();
	System.in.read();
//**/
    if (dups.size()>0){
    	StringBuffer sb = new StringBuffer();
    	for (Long l : dups) {
    		sb.append(l);
    		sb.append(" ");
    	}
    	Assert.fail("These ids are duplicated :"+sb.toString());
    }
    firstHalfSky.addAll(secondHalfSky);
    LongSet pixHalfsUnique = new LongSet(firstHalfSky);
    LongList pixHalfsList = new LongList(pixHalfsUnique);
    pixHalfsList = pixHalfsList.sort();
    fullSky = fullSky.sort();

    long listL = Math.min(fullSky.size(),pixHalfsList.size() );
    assertEquals(npix,fullSky.size());
    assertEquals(npix,listL);
    for ( int i=0; i< listL; i++) {

    assertEquals(fullSky.get(i),pixHalfsList.get(i));
    }
    


   firstHalfSky = new LongList(pt.queryDisc( new SpatialVector(1., 0., 0.), radius1+1e-12, inclusive));
   secondHalfSky = new LongList(pt.queryDisc( new SpatialVector(-1., 0., 0.),radius1-1e-12,  inclusive));
    firstHalfSky.addAll(secondHalfSky);
    pixHalfsUnique = new LongSet(firstHalfSky);
    pixHalfsList = new LongList(pixHalfsUnique);
    
    pixHalfsList = pixHalfsList.sort();
    System.out.println("full size="+fullSky.size()+" half size="+pixHalfsList.size());
    listL = Math.min(fullSky.size(),pixHalfsList.size() );
    assertEquals(npix,fullSky.size());
    assertEquals(npix,listL);
    for ( int i=0; i< listL; i++) {
//        System.out.println( "i="+i+" "+fullSky.get(i)+" "+pixHalfsList.get(i));
        assertEquals(fullSky.get(i),pixHalfsList.get(i));
        }


    firstHalfSky = new LongList(pt.queryDisc( new SpatialVector(0., 1., 0.), radius1+1e-12,  inclusive));
    secondHalfSky = new LongList(pt.queryDisc( new SpatialVector(0., -1., 0.), radius1-1e-12,  inclusive));
    firstHalfSky.addAll(secondHalfSky);
    pixHalfsUnique = new LongSet(firstHalfSky);
    pixHalfsList = new LongList(pixHalfsUnique);
    pixHalfsList = pixHalfsList.sort();
    System.out.println("full size="+fullSky.size()+" half size="+pixHalfsList.size());
    listL = Math.min(fullSky.size(),pixHalfsList.size() );
    assertEquals(npix,fullSky.size());

    for ( int i=0; i< listL; i++) {

        assertEquals(fullSky.get(i),pixHalfsList.get(i));
        }
        
}
	
	void dump(LongList sky) {
	    for (int p=0; p < sky.size(); p++) {
			System.out.print(" "+sky.get(p));
	    }
	    System.out.println();
	}
}
