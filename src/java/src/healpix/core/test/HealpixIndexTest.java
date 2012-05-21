package healpix.core.test;

import healpix.essentials.Zphi;
import healpix.essentials.HealpixUtils;
import healpix.core.HealpixIndex;
import healpix.tools.SpatialVector;
import junit.framework.TestCase;

public class HealpixIndexTest extends TestCase {

	public void testMaxPixrad() throws Exception {
		int nside = 128;
		HealpixIndex hi = new HealpixIndex(nside);
		double maxRad = hi.maxPixrad();
		assertTrue("Zero maxpixrad", maxRad>0);

	}

	public void testRing2z() throws Exception {
		int nside = 2048;
		HealpixIndex hi = new HealpixIndex(nside);
		SpatialVector v = new SpatialVector();
		double z =0.45, phi=0.7;
		v.set_z_phi(z, phi);
		long pix = hi.vec2pix_ring(v);
		int ring = hi.ring(pix);
		double zRing = hi.ring2z(ring);

		assertEquals("ringz does not agree",z, zRing, 0.01);



	}


	public void testPix2zphi() throws Exception {
		int nside = 2048;
		HealpixIndex hi = new HealpixIndex(nside);
		SpatialVector v = new SpatialVector();
		double z =0.45, phi=0.7;
		v.set_z_phi(z, phi);

		long pix = hi.vec2pix_nest(v);
		Zphi p= hi.pix2zphi(pix);

		assertEquals ("Z wrong ",z,p.z,0.001);


	}

	public void testIsqrt() {
		for (int r=2; r < 1000; r++)
		assertEquals(r,HealpixUtils.isqrt(r*r));
	}


}
