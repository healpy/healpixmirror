package healpix.core.dm.test;

import healpix.core.dm.HealpixMap.CoordSys;
import junit.framework.TestCase;

public class HealpixMapTest extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	public void testCoordSys() {
		
		String cs = " E ";
		assertTrue(cs.indexOf(CoordSys.ECLIPTIC.fitsType)>=0);
		CoordSys tcs = CoordSys.fromFits(cs.trim().charAt(0));
		assertEquals(CoordSys.ECLIPTIC, tcs);
		
		cs = " G ";
		assertTrue(cs.indexOf(CoordSys.GALACTIC.fitsType)>=0);
		tcs = CoordSys.fromFits(cs.trim().charAt(0));
		assertEquals(CoordSys.GALACTIC, tcs);

		cs = " C ";
		assertTrue(cs.indexOf(CoordSys.CELESTIAL.fitsType)>=0);
		tcs = CoordSys.fromFits(cs.trim().charAt(0));
		assertEquals(CoordSys.CELESTIAL, tcs);

	
	}
	protected void tearDown() throws Exception {
		super.tearDown();
	}

}
