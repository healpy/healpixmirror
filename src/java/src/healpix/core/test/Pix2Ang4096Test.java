package healpix.core.test;

import healpix.core.HealpixIndex;
import healpix.core.base.set.LongIterator;
import healpix.core.base.set.LongRangeSet;
import healpix.tools.SpatialVector;

import java.util.ArrayList;

import junit.framework.TestCase;

/**
 * The Class Pix2Ang4096Test.
 */
public class Pix2Ang4096Test extends TestCase {

	/**
	 * Test pix2 ang.
	 * 
	 * @throws Exception the exception
	 */
	public void testPix2Ang() throws Exception {
		System.out.println(" Test starts... ");
		double PI = Math.PI;
		double deg2rad = PI / 180;
		long pix = -1;
		// alpha,delta => healpix coord: [theta,phi] = [90-delta, alpha]
		double alpha = 30; // Ra
		double delta = 80; // Dec
		System.out.println("[alpha,delta] input: [" + alpha + ", " + delta
				+ " ]");
		double theta = deg2rad * (90 - delta); // in rad
		double phi = deg2rad * alpha; // in rad
		System.out.println("[theta,phi] healpix input: [" + (90 - delta) + ", "
				+ alpha + " ]");
		int nside = 4096; // 2^12
		HealpixIndex pt = new HealpixIndex(nside);
		try {
			pix = pt.ang2pix_nest(theta, phi);
			System.out.println("Ipix_nest=" + pix);
		} catch (Exception e) {
			e.printStackTrace();
		}
		SpatialVector v = (SpatialVector) HealpixIndex.ang2Vec(theta, phi);
		long pix1 = pt.vec2pix_nest(v);
		System.out.println("** ipix1 (from angular spatial vector)=" + pix1);
		double[] radec2 = pt.pix2ang_nest(pix1);
		System.out.println("** Theta,phi from ipix1:" + radec2[0] / deg2rad + ","
				+ radec2[1] / deg2rad);

		assertEquals(pix1, pix);
		long pix2test = pix;// 83188373;//80568190;//16377824;
		double[] radec_nest = pt.pix2ang_nest( pix2test);
		System.out.println("** @ ipix=" + pix2test + " [theta,phi]: ["
				+ radec_nest[0] / deg2rad + "," + radec_nest[1] / deg2rad
				+ " ]");
		// takes copy in constructor...
		SpatialVector vec = new SpatialVector(v);// 10, 30);
		long pix3 = pt.vec2pix_nest(vec);
		System.out.println(" Nside =" + nside + " Ra=30,dec=10(90-delta) pix=" + pix3);
		System.out.println("vec RA:" + vec.ra()+", DEC:"+vec.dec());
		assertEquals(pix3, pix);		
		// takes RA DEC in constructor...
		SpatialVector vec2 = new SpatialVector(alpha, delta);
		long pix4 = pt.vec2pix_nest(vec2);
		assertEquals(pix4, pix);
	}
	
	/**
	 * Query_ polygon.
	 * 
	 * @throws Exception the exception
	 */
	@SuppressWarnings({ "unused", "unchecked"})
	public void Query_Polygon() throws Exception {
		double PI = Math.PI;
		double deg2rad = PI / 180;
		int nside = 4;
		HealpixIndex pt = new HealpixIndex((int) nside);
		
		double ang1[]={90*deg2rad, -1*deg2rad};
		double ang2[]={90*deg2rad, -1*deg2rad};
		double ang3[]={95*deg2rad, 1*deg2rad};
		double ang4[]={85*deg2rad, 1*deg2rad};
		SpatialVector v1 = HealpixIndex.ang2Vec(ang1[0],ang1[1]);
		SpatialVector v2 = HealpixIndex.ang2Vec(ang2[0],ang2[1]);
		long ipix1 = pt.ang2pix_nest(ang1[0],ang1[1]);
		long ipix2 = pt.ang2pix_nest(ang2[0],ang2[1]);
		SpatialVector v3 = HealpixIndex.ang2Vec(ang3[0],ang3[1]);
		SpatialVector v4 = HealpixIndex.ang2Vec(ang4[0],ang4[1]);
		long ipix3 = pt.ang2pix_nest(ang3[0],ang3[1]);
		long ipix4 = pt.ang2pix_nest(ang4[0],ang4[1]);
		System.out.println("Ipx1:"+ipix1);
		System.out.println("Ipx2:"+ipix2);
//		
		int nest = 0;
		long ipix = 0;
//		int[] result = null;
		int inclusive = 1;
		int[] result = { 51, 52, 53, 66, 67, 68, 69, 82, 83, 84, 85, 86, 98,
				99, 100, 101, 115, 116, 117 };
//		int[] result1 = { 55, 70, 71, 87 };
//		int[] result2 = { 137, 152, 153, 168 };
//		int[] result3 = { 27, 43, 44, 58, 59, 60, 74, 75, 76, 77, 89, 90, 91,
//				92, 93, 105, 106, 107, 108, 109, 110, 121, 122, 123, 124, 125,
//				138, 139, 140, 141, 154, 156 };
//
		System.out.println("Start test query_polygon !!!!!!!!!!!!!!!!!!!!!!");
		ArrayList vlist = new ArrayList();
		vlist.add((Object) v1);
		vlist.add((Object) v2);
		vlist.add((Object) v3);
//		vlist.add((Object) v4);
//		v = pt.pix2vec_ring(82);
//		vlist.add((Object) v);
//		v = pt.pix2vec_ring(115);
//		vlist.add((Object) v);
//		v = pt.pix2vec_ring(117);
//		vlist.add((Object) v);
//		v = pt.pix2vec_ring(86);
//		vlist.add((Object) v);
//
//		ArrayList pixlist;
		LongRangeSet pixlist;
		pixlist = pt.query_polygon(nside, vlist, nest, inclusive);
		System.out.println(" List size="+pixlist.size());
		LongIterator it = pixlist.longIterator();
		int i=0;
		while(it.hasNext()) {
			ipix = it.next();
//			assertEquals("pixel = " + ipix, result[i], ipix, 1e-10);
			System.out.println("i="+i+" pixel # "+ipix);
			i++;
		}
//
//		/* Yet another test */
//
//		ArrayList vlist1 = new ArrayList();
//		v = pt.pix2vec_ring(71);
//		vlist1.add((Object) v);
//		v = pt.pix2vec_ring(55);
//		vlist1.add((Object) v);
//		v = pt.pix2vec_ring(70);
//		vlist1.add((Object) v);
//		v = pt.pix2vec_ring(87);
//		vlist1.add((Object) v);
//		pixlist = pt.query_polygon(nside, vlist1, nest, inclusive);
//
//		nlist = pixlist.size();
//
//		for (int i = 0; i < nlist; i++) {
//			ipix = ((Long) pixlist.get(i)).longValue();
//			// System.out.println("i="+i+" pixel # "+ipix);
//			assertEquals("pixel = " + ipix, result1[i], ipix, 1e-10);
//		}
//
//		/* Yet another test */
//		ArrayList vlist2 = new ArrayList();
//		v = pt.pix2vec_ring(153);
//		vlist2.add((Object) v);
//		v = pt.pix2vec_ring(137);
//		vlist2.add((Object) v);
//		v = pt.pix2vec_ring(152);
//		vlist2.add((Object) v);
//		v = pt.pix2vec_ring(168);
//		vlist2.add((Object) v);
//		pixlist = pt.query_polygon(nside, vlist2, nest, inclusive);
//
//		nlist = pixlist.size();
//
//		for (int i = 0; i < nlist; i++) {
//			ipix = ((Long) pixlist.get(i)).longValue();
//			assertEquals("pixel = " + ipix, result2[i], ipix, 1e-10);
//			// System.out.println("i="+i+" pixel # "+ipix);
//		}
//		/* Yet another test */
//
//		ArrayList vlist3 = new ArrayList();
//		v = pt.pix2vec_ring(110);
//		vlist3.add((Object) v);
//		v = pt.pix2vec_ring(27);
//		vlist3.add((Object) v);
//		v = pt.pix2vec_ring(105);
//		vlist3.add((Object) v);
//		v = pt.pix2vec_ring(154);
//		vlist3.add((Object) v);
//		v = pt.pix2vec_ring(123);
//		vlist3.add((Object) v);
//		v = pt.pix2vec_ring(156);
//		vlist3.add((Object) v);
//		pixlist = pt.query_polygon(nside, vlist3, nest, inclusive);
//
//		nlist = pixlist.size();
//
//		for (int i = 0; i < nlist; i++) {
//			ipix = ((Long) pixlist.get(i)).longValue();
//			assertEquals("pixel = " + ipix, result3[i], ipix, 1e-10);
//			// System.out.println("i="+i+" pixel # "+ipix);
//		}
//		System.out.println(" test query_polygon is done");

	}
}
