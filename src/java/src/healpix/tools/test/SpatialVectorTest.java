package healpix.tools.test;

import healpix.core.HealpixIndex;
import healpix.tools.SpatialVector;

import junit.framework.TestCase;

/**
 * The Class SpatialVectorTest.
 */
public class SpatialVectorTest extends TestCase {

	/**
	 * Test dot product.
	 */
	public void testDotProduct() {
		SpatialVector v1 = new SpatialVector(1, 0, 0);
		SpatialVector v2 = new SpatialVector(0, 1, 0);
		SpatialVector v3 = new SpatialVector(-1, 0, 0);

		// v1 perpendicular to v2

		double res = v1.dot(v2);
		assertEquals(res, 0.d);
		// Get angle: has to be 90 deg.(pi/2 in rad)
		assertEquals(Math.acos(res), Math.PI / 2);

		// v1 oposite to v3

		double res2 = v1.dot(v3);
		assertEquals(res2, -1.d);

	}

	/**
	 * Test length.
	 */
	public void testLength() {
		SpatialVector v1 = new SpatialVector(1, 1, 1);
		assertEquals(v1.length(), Math.sqrt(3.d));
		v1.normalized();
		assertEquals(v1.length(), 1.d);
		v1 = new SpatialVector(2, 0, 0);
		assertEquals(v1.length(), 2.d);
	}

	/**
	 * Test norm.
	 */
	public void testNorm() {
		SpatialVector v1 = new SpatialVector(2, 1, 1);
		SpatialVector v2 = new SpatialVector(2,1,1);

		System.out.println("Vector before norm:"+v1.toString());
		assertEquals(v1.length(), Math.sqrt(6.d));
		v1.normalized();
		System.out.println("SpatialVector after norm:"+v1.toString());
		v2.normalized();
		System.out.println("Vector3d after norm:"+v2.toString());
	}

	/**
	 * Test me.
	 */
	public void testMe() {
		SpatialVector v = new SpatialVector(0.9998476951563913,
				-0.01745240643728351, 6.123233995736766E-17);
		v.normalized();
		System.out.println("Vector norm:"+v.toString());
		long nside = 4;
		HealpixIndex pt;
		try {
			pt = new HealpixIndex((int) nside);
			v = pt.pix2vec_ring(51);
			System.out.println("Vector 51 ang RA, DEC:"+v.ra() +", "+v.dec());
			System.out.println("Vector x,y,z:"+v.toString());
			v = pt.pix2vec_ring(50);
			System.out.println("Vector 50 ang RA, DEC:"+v.ra() +", "+v.dec());
			System.out.println("Vector x,y,z:"+v.toString());
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}
}
