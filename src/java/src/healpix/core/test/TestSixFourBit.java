package healpix.core.test;

import healpix.core.AngularPosition;
import healpix.core.Healpix;
import healpix.core.HealpixIndex;
import healpix.tools.Constants;

import java.text.DecimalFormat;

import junit.framework.TestCase;

/** @deprecated */
public class TestSixFourBit extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * try the nside cds use
	 * @throws Exception
	 */
	public void test262144() throws Exception {
		// CDS - Pierre Fernique uses this NSIDE
		// nolnger can do this ..
		//testSome(262144, false);
	}
	
	/**
	 * Test pix to angle and angle to pixel tied to a resolution number nside
	 * Do this for some pixels in each face .. assuming big HEALPIx nums can not do 
	 * for all ..
	 * 
	 * @param nside
	 *            resolution number (big one > 8192)
	 * @param ring
	 *            if true, ring scheme is selected
	 * @throws Exception
	 */
	public void testSome(int nside, boolean ring) throws Exception {

		HealpixIndex hi = new HealpixIndex(nside);

		long nsf = (long)nside*(long)nside;
		long length = 12 * nsf;
		DecimalFormat form = new DecimalFormat("#.###");
		int nside32 = 4096;
		boolean downgrade=true;
		if (nside < nside32) {
			nside32=nside ;
			downgrade=false;
		}

		System.err.println("Doing NSIDE="+nside+" npix="+length + " with nside32="+nside32+ " dgrade="+downgrade);
		for (long i = 0; i < length; i +=nsf) {
			AngularPosition pos = null;
			double[] posHi = null;

			// get parent pixel - use that for a sanity check in 32bit version
			// if needed 
			long pix32 = i;
			if (ring) {
				long npix = hi.ring2nest(i);
				if (downgrade) {
				    pix32 = HealpixIndex.parentAt(npix, nside, nside32);
			    	pix32 = Healpix.nest2ring(nside32, (int) pix32);
				}
				pos = Healpix.pix2ang_ring(nside32, (int) pix32);				
				posHi = hi.pix2ang_ring(i);
			} else {
				if (downgrade) {
					pix32= HealpixIndex.parentAt(i, nside, nside32);
				}
				pos = Healpix.pix2ang_nest(nside32, (int) pix32);

				posHi = hi.pix2ang_nest(i);
			}
			int pix = 0;
			long lpix =0;
			if (ring){
				pix = Healpix.ang2pix_ring(nside, pos.theta(), pos.phi());
				lpix = hi.ang2pix_ring( posHi[0], posHi[1]);
				
			}else{
				pix = Healpix.ang2pix_nest(nside32, pos.theta(), pos.phi());
				lpix = hi.ang2pix_nest( posHi[0], posHi[1]);
			}
			
			assertEquals("Healpix and HealpixIndex disagree on theta for " + i,
					pos.theta(), posHi[0],0.001); 
			// guess could use precisson of pix to figure this accuracy out  
			
			assertEquals("pix32 incorrect for  theta "
					+ form.format(Math.cos(pos.theta())) + " phi/pi "
					+ form.format(pos.phi() / Constants.PI), pix32, pix);

			assertEquals("i incorrect for  theta "
					+ form.format(Math.cos(posHi[0])) + " phi/pi "
					+ form.format(posHi[1] / Constants.PI), i, lpix);

			assertEquals("Healpix and HealpixIndex disagree on phi", pos.phi(),
					posHi[1],0.0001);

		}

	}

}
