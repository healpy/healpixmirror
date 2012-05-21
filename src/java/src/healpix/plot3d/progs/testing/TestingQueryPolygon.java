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
package healpix.plot3d.progs.testing;

import healpix.core.AngularPosition;
import healpix.core.HealpixIndex;
import healpix.core.base.set.LongIterator;
import healpix.core.base.set.LongRangeSet;
import healpix.core.dm.HealpixMap;
import healpix.essentials.Scheme;
import healpix.plot3d.gui.view.MapView3d;
import healpix.tools.HealpixMapCreator;
import healpix.tools.SpatialVector;

import java.util.ArrayList;
import java.util.List;

/**
 * Testing query polygon to detect prime meridian black zone defect in Planck
 * Usage: java -cp jhealpix.jar healpix.plot3d.progs.testing.TestingQueryPolygon
 *
 * @author ejoliet
 * @version $Id: Healpix3DMapViewer.java 26131 2007-06-27 16:02:03Z ejoliet $
 */
public class TestingQueryPolygon {

	/** The vlist. */
	private static ArrayList<SpatialVector> vlist;

	/**
	 * The main method.
	 *
	 * @param args
	 *            the arguments
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static void main(String[] args) throws Exception {
		try {
			MapView3d mview = new MapView3d(false);
			vlist = new ArrayList<SpatialVector>();
//			 HealpixMap map = getMapWithPixRingTriangle();
			HealpixMap map = getMapWithPixNest();
//			 HealpixMap map = getMap3PixelsRing();
//			 HealpixMap map = getMapNeighbours();
			// // getMap();
			mview.setMap(map);
			System.out.println("Map min/max: " + map.getMin(0) + "/"
					+ map.getMax(0));
			mview.setSize(800, 800);
			mview.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Gets the map.
	 *
	 * @return the map
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static HealpixMap getMap() throws Exception {
		int nside = 4;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);
		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.NESTED);
		HealpixIndex hi = new HealpixIndex(nside);
		SpatialVector vec1 = new SpatialVector();
		double offsetra = 0.0;
		double offsetdec = 0.0;
		// make left polygon
		vec1.set((345.0 + offsetra), 10.0 + offsetdec);// north
		SpatialVector vec2 = new SpatialVector();
		System.out.println("1:" + hi.vec2pix_nest(vec1));
		vec2.set((350.0 + offsetra), -20.0 + offsetdec);// south
		System.out.println("2:" + hi.vec2pix_nest(vec2));
		// hemisphere
		SpatialVector vec3 = new SpatialVector();
		vec3.set((20.0 + offsetra), -20.0 + offsetdec);// South hemisphere
		System.out.println("3:" + hi.vec2pix_nest(vec3));
		SpatialVector vec4 = new SpatialVector();
		vec4.set((20.0 + offsetra), 10.0 + offsetdec);
		System.out.println("4:" + hi.vec2pix_nest(vec4));

		vlist.add(vec1);
		vlist.add(vec2);
		vlist.add(vec3);
		vlist.add(vec4);

		LongRangeSet pixlist = new HealpixIndex(map.nside()).query_polygon(map
				.nside(), vlist, 1, 0);
		pixlist = new HealpixIndex(map.nside()).query_triangle(map.nside(),
				vec1, vec2, vec3, 1, 0);
		LongIterator it = pixlist.longIterator();
		while(it.hasNext()){
			map.setValueCell(((Long)it.next()).intValue(), 0.5);
			// System.out.println(ip);
		}

		addVec(vec1, map, 1);
		addVec(vec2, map, 2);
		addVec(vec3, map, 3);
		addVec(vec4, map, 4);
		return map;
	}

	/**
	 * Gets the map with pix ring triangle.
	 *
	 * @return the map with pix ring triangle
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static HealpixMap getMapWithPixRingTriangle() throws Exception {
		int nside = 4;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);

		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.NESTED);

		HealpixIndex pt = new HealpixIndex(nside);
		int nest = 0;
		int inclusive = 1;
		int triang[] = { 71, 135, 105 }; // crossing 360
		// because center
		// outside the triangle
		// (inclusive=0)
		System.out.println("Start test Query Triangle");
		SpatialVector v[] = new SpatialVector[3];

		for (int vi = 0; vi < v.length; vi++) {
			map.add((int) triang[vi], 10);
			v[vi] = pt.pix2vec_nest(triang[vi]);

		}

//		ArrayList pixlist;
		LongRangeSet pixlist;
		pixlist = pt.query_triangle(nside, v[0], v[1], v[2], nest, inclusive);
		LongIterator it = pixlist.longIterator();
		while(it.hasNext()){
			long ip = ((Long) it.next()).longValue();
			map.add((int) pt.ring2nest(ip), 5);
			System.out.println(ip);
		}

		return map;

	}

	/**
	 * Gets the map3 pixels ring.
	 *
	 * @return the map3 pixels ring
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static HealpixMap getMap3PixelsRing() throws Exception {
		int nside = 4;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);
		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.NESTED);
		// int[] ipringtest = {19,13,28};//crossing
		int[] ipringtest = { 71, 135, 105 };// crossing (nside = 4)
		HealpixIndex pt = new HealpixIndex(nside);
		System.out.println("Start test Query Triangle");
		SpatialVector v[] = new SpatialVector[3];
		for (int i = 0; i < ipringtest.length; i++) {
			int iptest = (int) pt.ring2nest(ipringtest[i]);
			v[i] = pt.pix2vec_ring(ipringtest[i]);
			double[] ang = pt.pix2ang_nest(iptest);
			System.out.println(ang[0] + "," + ang[1]);
			map.add(iptest, 10);
		}

//		ArrayList pixlist;
		LongRangeSet pixlist;
		pixlist = pt.query_triangle(nside, v[0], v[1], v[2], 0, 0);
		LongIterator it = pixlist.longIterator();
		while(it.hasNext()){
			long ip = ((Long) it.next()).longValue();
			map.add((int) pt.ring2nest( ip), 5);
			System.out.println(ip);
		}
		return map;
	}

	/**
	 * Gets the map with pix ring.
	 *
	 * @return the map with pix ring
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static HealpixMap getMapWithPixRing() throws Exception {
		int nside = 4;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);

		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.RING);

		ArrayList<SpatialVector> vlist1 = new ArrayList<SpatialVector>();
		SpatialVector v = null;
		HealpixIndex pt = new HealpixIndex(nside);

		double pv = 3;
		v = pt.pix2vec_ring(1);
		vlist1.add(v);
		map.setValueCell((int) 1, 9);

		addVec(v, map, pv++);
		v = pt.pix2vec_ring(48);
		vlist1.add(v);
		addVec(v, map, pv++);
		v = pt.pix2vec_ring(94);
		vlist1.add(v);
		addVec(v, map, pv++);
		v = pt.pix2vec_ring(112);
		vlist1.add(v);
		addVec(v, map, pv++);
		v = pt.pix2vec_ring(81);
		vlist1.add(v);
		addVec(v, map, pv++);

		LongRangeSet pixlist = pt.query_polygon(nside, vlist1, 0, 0);
		LongIterator it = pixlist.longIterator();
		while(it.hasNext()){
			Long ip = it.next();
			map.add( ip.intValue(), 0.5);
			System.out.println(ip);
		}

		return map;

	}

	/**
	 * Gets the map with pix nest.
	 *
	 * @return the map with pix nest
	 *
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	public static HealpixMap getMapNeighbours() throws Exception {
		int nside = 32;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);
		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.NESTED);

		ArrayList vlist1 = new ArrayList();
		SpatialVector v = null;
		HealpixIndex pt = new HealpixIndex(nside);

		double pv = 3;
		int[] arr = new int[] { 4444, 4446, 4468, 4469, 32, 10, 8, 4445 };
//		int[] arr = new int[] { 4444, 4446, 4468, 4469, 1056, 1034, 1032, 4445 };

		for (int i = 0; i < arr.length; i++) {
			v = pt.pix2vec_nest(arr[i]);
			vlist1.add((Object) v);
			addVec(nside,v, map, pv);
			map.setValueCell(arr[i], pv);
		}
		v = pt.pix2vec_nest(4447);
		vlist1.add((Object) v);
		map.setValueCell(4447, pv);
		addVec(nside,v, map, pv*2);
		List<Long> pixlist = pt.neighbours_nest( 4447);
		int nlist = pixlist.size();
		for (int i = 0; i < nlist; i++) {
			long ip = (int) ((Long) pixlist.get(i)).longValue();
			//map.add((int) ip, 0.5);
			System.out.println(ip);
		}

		return map;

	}

	/**
	 * Gets the map with pix nest.
	 *
	 * @return the map with pix nest
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static HealpixMap getMapWithPixNest() throws Exception {
		int nside = 4;
		HealpixMapCreator cr = new HealpixMapCreator(nside, true);
		HealpixMap map = cr.getMap();
		map.setScheme(Scheme.NESTED);

		ArrayList<SpatialVector> vlist1 = new ArrayList<SpatialVector>();
		SpatialVector v = null;
		HealpixIndex pt = new HealpixIndex(nside);

		double pv = 3;
		v = pt.pix2vec_nest(21);
		vlist1.add(v);
		map.setValueCell((int) 21, pv);
		addVec(v, map, pv++);
		v = pt.pix2vec_nest(16);
		vlist1.add(v);
		map.setValueCell(16, pv);
		addVec(v, map, pv++);
		v = pt.pix2vec_nest(104);
		vlist1.add(v);
		map.setValueCell(104, pv);
		addVec(v, map, pv++);
		v = pt.pix2vec_nest(109);
		map.setValueCell(109, pv);
		vlist1.add(v);
		addVec(v, map, pv++);

		addVec(v, map, pv++);
		LongRangeSet pixlist = pt.query_polygon(nside, vlist1, 1, 0);
		LongIterator it = pixlist.longIterator();
		while(it.hasNext()){
			Long ip = it.next();
			map.add(ip.intValue(), 0.5);
			System.out.println(ip);
		}

		return map;

	}

	/**
	 * Adds the vec.
	 *
	 * @param vec
	 *            the vec
	 * @param map
	 *            the map
	 * @param v
	 *            the v
	 *
	 * @throws Exception
	 *             the exception
	 */
	public static void addVec(SpatialVector vec, HealpixMap map,
			double v) throws Exception {

		double angs[] = HealpixIndex.vec2Ang(vec);

		AngularPosition ang2 = new AngularPosition(angs[0], angs[1]);

		map.add(ang2, v);

	}

	public static void addVec(int nside, SpatialVector vec, HealpixMap map,
			double v) throws Exception {

		double angs[] = HealpixIndex.vec2Ang(vec);

		AngularPosition ang2 = new AngularPosition(angs[0], angs[1]);

		map.add(ang2, v);

	}
}
