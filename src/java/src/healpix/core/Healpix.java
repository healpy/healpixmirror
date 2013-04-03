/*
 *  HEALPix Java code original port for Gaia by wil.
 *  This code is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This code is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this code; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  For more information about HEALPix, see http://healpix.sourceforge.net

 *
 */
package healpix.core;

import healpix.tools.Constants;
import healpix.tools.SpatialVector;

import java.awt.Point;
import java.util.ArrayList;

/**
 * Java version of some healpix routines from DSRI in java everthing must be in
 * a class - no functions floating about. Original algorithms Eric Hivon and
 * Krzysztof M. Gorski. Healpix.core.Healpixmap does this for a fixed nside This
 * code written by William O'Mullane
 * 
 * THIS IS 32 BIT - if you want 64 use HealpixIndex
 * 
 * @deprecated Use HealpixIndex - this class will be deleted and is no longer compatible
 * with the 64bit version.
 * 
 * @version $Id: Healpix.java 112179 2009-11-18 10:29:01Z womullan $
 * @author womullan
 * 
 */
public final class Healpix {

	// should work for 8192 but apparently there is some problem
	// if you want more than this use HealpixIndex which is 64bit
	/** The Constant ns_max. */
	public static final int ns_max = 4096;

	/** The Constant x2pix. */
	private static final int x2pix[] = new int[128];

	/** The Constant y2pix. */
	private static final int y2pix[] = new int[128];

	/** The Constant pix2x. */
	private static final int pix2x[] = new int[1024];

	/** The Constant pix2y. */
	private static final int pix2y[] = new int[1024];


	/**
	 * Initialize pix2x and pix2y constructs the array giving x and y in the
	 * face from pixel number for the nested (quad-cube like) ordering of pixels
	 * the bits corresponding to x and y are interleaved in the pixel number one
	 * breaks up the pixel number by even and odd bits
	 */
	protected static void mkpix2xy() {
		int kpix, jpix, ix, iy, ip, id;

		for (kpix = 0; kpix <= 1023; kpix++) { // pixel number
			jpix = kpix;
			ix = 0;
			iy = 0;
			ip = 1;// bit position (in x and y)
			while (jpix != 0) { // go through all the bits;
				id = jpix % 2;// bit value (in kpix), goes in ix
				jpix = jpix / 2;
				ix = id * ip + ix;

				id = jpix % 2;// bit value (in kpix), goes in iy
				jpix = jpix / 2;
				iy = id * ip + iy;

				ip = 2 * ip;// next bit (in x and y)
			}
			;
			pix2x[kpix] = ix;// in 0,31
			pix2y[kpix] = iy;// in 0,31
		}
		;

	}

	/**
	 * Initialize x2pix and y2pix
	 */
	protected static void mkxy2pix() {
		int j, k, id, ip;
		for (int i = 0; i < 128; i++) {
			j = i;
			k = 0;
			ip = 1;
			while (j != 0) {
				id = j % 2;
				j = j / 2;
				k = ip * id + k;
				ip = ip * 4;
			}
			x2pix[i] = k;
			y2pix[i] = 2 * k;
		}
	}

	/**
	 * renders the pixel number ipix (NESTED scheme) for a pixel which contains
	 * a point on a sphere at coordinates theta and phi, given the map
	 * resolution parametr nside the computation is made to the highest
	 * resolution available (nside=8192) and then degraded to that required (by
	 * integer division) this doesn't cost more, and it makes sure that the
	 * treatement of round-off will be consistent for every resolution
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]	 * @return pixel index nested
	 * @throws Exception
	 */
	public static final  int  ang2pix_nest(int nside, double theta, double phi)
			throws Exception {
		int ipix;
		double z, za, z0, tt, tp, tmp;
		int jp, jm, ifp, ifm, face_num, ix, iy;
		int ix_low, ix_hi, iy_low, iy_hi, ipf, ntt;

		if (phi >= Constants.twopi)
			phi = phi - Constants.twopi;
		if (phi < 0.)
			phi = phi + Constants.twopi;
		if (nside > ns_max || nside < 1) {
			throw new Exception("nsides must be between 1 and " + ns_max);
		}
		if (theta > Constants.PI || theta < 0) {
			throw new Exception("theta must be between 0 and " + Constants.PI);
		}
		if (phi > Constants.twopi || phi < 0) {
			throw new Exception("phi must be between 0 and " + Constants.twopi);
		}
		// Note excpetion thrown means method does not get further.
		if (y2pix[127] == 0)
			mkxy2pix();

		z = Math.cos(theta);
		za = Math.abs(z);
		z0 = 2.0 / 3.0;
		tt = phi / Constants.piover2;// in [0,4]

		// System.out.println("Za:"+za +" z0:"+z0+" tt:"+tt+" z:"+z+"
		// theta:"+theta+" phi:"+phi);
		if (za <= z0) { // Equatorial region
			// System.out.println("Equatorial !");
			// (the index of edge lines increase when the longitude=phi goes up)
			jp = (int) Math.rint(ns_max * (0.50 + tt - (z * 0.750)));// ascending
			// edge
			// line
			// index
			jm = (int) Math.rint(ns_max * (0.50 + tt + (z * 0.750)));// descending
			// edge
			// line
			// index

			// finds the face
			ifp = jp / ns_max; // in {0,4}
			ifm = jm / ns_max;
			if (ifp == ifm) { // faces 4 to 7
				face_num = (ifp % 4) + 4;
			} else {
				if (ifp < ifm) { // (half-)faces 0 to 3
					face_num = (ifp % 4);
				} else { // (half-)faces 8 to 11
					face_num = (ifm % 4) + 8;
				}
				;
			}
			;

			ix = (jm % ns_max);
			iy = ns_max - (jp % ns_max) - 1;
		} else { // polar region, za > 2/3

			ntt = (int) (tt);
			if (ntt >= 4)
				ntt = 3;
			tp = tt - ntt;
			tmp = Math.sqrt(3.0 * (1.0 - za)); // in ]0,1]

			// (the index of edge lines increase when distance from the closest
			// pole goes up)
			jp = (int) Math.rint(ns_max * tp * tmp);// line going toward the
			// pole as phi increases
			jm = (int) Math.rint(ns_max * (1.0 - tp) * tmp); // that one goes
			// away of the
			// closest pole
			jp = Math.min(ns_max - 1, jp); // for points too close to the
			// boundary
			jm = Math.min(ns_max - 1, jm);

			// finds the face and pixel's (x,y)
			if (z >= 0) { // North Pole
				// System.out.println("Polar z>=0 ntt:"+ntt+" tt:"+tt);
				face_num = ntt; // in {0,3}
				ix = ns_max - jm - 1;
				iy = ns_max - jp - 1;
			} else {
				// System.out.println("Polar z<0 ntt:"+ntt+" tt:"+tt);
				face_num = ntt + 8;// in {8,11}
				ix = jp;
				iy = jm;
			}
			;
		}
		;

		ix_low = ix % 128;
		ix_hi = ix / 128;
		iy_low = iy % 128;
		iy_hi = iy / 128;
		ipf = (x2pix[ix_hi] + y2pix[iy_hi]) * (128 * 128)
				+ (x2pix[ix_low] + y2pix[iy_low]); // in {0, nside**2 - 1}

		ipf = ipf / (int) Math.rint((Math.pow((ns_max / nside), 2)));
		// System.out.println("ix_low:"+ix_low +" ix_hi:"+ix_hi+"
		// iy_low:"+iy_low+" iy_hi:"+iy_hi+" ipf:"+ipf+ " face:"+face_num);

		ipix = (int) Math.rint(ipf + face_num * Math.pow(nside, 2)); // in
		// {0,
		// 12*nside**2
		// - 1}

		return ipix;

	}

	/**
	 * Convert from pix number to angle renders theta and phi coordinates of the
	 * nominal pixel center for the pixel number ipix (NESTED scheme) given the
	 * map resolution parameter nside
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipix
	 *            index in nest scheme
	 * @return angular position
	 * @throws Exception
	 */
	public static final AngularPosition pix2ang_nest(int nside, int ipix)
			throws Exception {

		long npix, npface, ipf; 
		int  ip_low, ip_trunc, ip_med,ip_hi, face_num;
		int jrt, jr, nr, jpt, jp, kshift, nl4, ix, iy;
		double z, fn, fact1, fact2, theta, phi;

		// cooordinate of the lowest corner of each face
		// add extra zero in front so array like in fortran
		int jrll[] = { 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 };
		int jpll[] = { 0, 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 };
		// -----------------------------------------------------------------------
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		npix = (int) (12 * Math.pow(nside, 2));
		if (ipix < 0 || ipix > npix - 1)
			throw new Exception("ipix out of range");

		// initiates the array for the pixel number -> (x,y) mapping
		if (pix2x[1023] <= 0)
			mkpix2xy();

		fn = 1.0 * nside;
		fact1 = 1.0 / (3.0 * fn * fn);
		fact2 = 2.0 / (3.0 * fn);
		nl4 = 4 * nside;

		// finds the face, and the number in the face
		npface = (int) Math.pow(nside, 2);

		face_num = (int)(ipix / npface);// face number in {0,11}
		ipf = (int) (ipix % npface);// pixel number in the face {0,npface-1}

		// finds the x,y on the face (starting from the lowest corner)
		// from the pixel number
		ip_low = (int) (ipf % 1024); // content of the last 10 bits
		ip_trunc = (int)(ipf / 1024); // truncation of the last 10 bits
		ip_med = (int)(ip_trunc % 1024); // content of the next 10 bits
		ip_hi = (int)(ip_trunc / 1024); // content of the high weight 10 bits

		ix = 1024 * pix2x[ip_hi] + 32 * pix2x[ip_med] + pix2x[ip_low];
		iy = 1024 * pix2y[ip_hi] + 32 * pix2y[ip_med] + pix2y[ip_low];

		// transforms this in (horizontal, vertical) coordinates
		jrt = ix + iy;// 'vertical' in {0,2*(nside-1)}
		jpt = ix - iy;// 'horizontal' in {-nside+1,nside-1}

		// computes the z coordinate on the sphere
		jr = jrll[face_num + 1] * nside - jrt - 1;
		// ring number in {1,4*nside-1}

		nr = nside;// equatorial region (the most frequent)
		z = (2 * nside - jr) * fact2;
		kshift = (jr - nside) % 2;
		if (jr < nside) { // north pole region
			nr = jr;
			z = 1.0 - nr * nr * fact1;
			kshift = 0;
		} else {
			if (jr > 3 * nside) { // south pole region
				nr = nl4 - jr;
				z = -1.0 + nr * nr * fact1;
				kshift = 0;
			}
		}
		;
		theta = Math.acos(z);

		// computes the phi coordinate on the sphere, in [0,2Pi]
		jp = (jpll[face_num + 1] * nr + jpt + 1 + kshift) / 2;// 'phi' number
		// in the ring
		// in {1,4*nr}
		if (jp > nl4)
			jp = jp - nl4;
		if (jp < 1)
			jp = jp + nl4;

		phi = (jp - (kshift + 1) * 0.50) * (Constants.piover2 / nr);

		return new AngularPosition(theta, phi);

	}

	/**
	 * Convert from pix number to angle renders theta and phi coordinates of the
	 * nominal pixel center for the pixel number ipix (RING scheme) given the
	 * map resolution parameter nside *
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipix
	 *            index in ring scheme
	 * @return angular position
	 * @throws Exception
	 */
	public static final AngularPosition pix2ang_ring(int nside, int ipix)
			throws Exception {

		double theta, phi;
		long nl2, nl4, npix, ncap, iring, iphi, ip, ipix1;
		double fact1, fact2, fodd, hip, fihip;
		// -----------------------------------------------------------------------
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		npix = (int) (12 * Math.pow(nside, 2)); // total number of points
		if (ipix < 0 || ipix > npix - 1)
			throw new Exception("ipix out of range");

		ipix1 = ipix + 1;// in {1, npix}
		nl2 = 2 * nside;
		nl4 = 4 * nside;
		ncap = 2 * nside * (nside - 1);// points in each polar cap, =0 for
		// nside =1
		fact1 = 1.50 * nside;
		fact2 = 3.0 * Math.pow(nside, 2);

		if (ipix1 <= ncap) { // North Polar cap -------------

			hip = ipix1 * 0.5;
			fihip = (int) (hip);
			iring = (int) (Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
			// from
			// North
			// pole
			iphi = ipix1 - 2 * iring * (iring - 1);

			theta = Math.acos(1.0 - Math.pow(iring, 2) / fact2);
			phi = ((double) (iphi) - 0.50) * Constants.PI / (2.0 * iring);

		} else {
			if (ipix1 <= nl2 * (5 * nside + 1)) { // Equatorial region ------
				ip = ipix1 - ncap - 1;
				iring = (int) (ip / nl4) + nside;// counted from North pole
				iphi = (int) ip % nl4 + 1;

				fodd = 0.50 * (1 + ((iring + nside) % 2)); // 1 if iring+nside
				// is odd, 1/2
				// otherwise
				theta = Math.acos((nl2 - iring) / fact1);
				phi = ((double) (iphi) - fodd) * (Constants.PI / (double) (nl2));

			} else { // South Polar cap -----------------------------------
				ip = npix - ipix1 + 1;
				hip = ip / 2.0;
				fihip = (int) (hip);
				iring = (int) (Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
				// from
				// South
				// pole
				iphi = 4 * iring + 1 - (ip - 2 * iring * (iring - 1));

				theta = Math.acos(-1.0 + Math.pow(iring, 2) / fact2);
				phi = ((double) (iphi) - 0.50) * Constants.PI / (double) (2.0 * iring);

			}
		}
		;

		return new AngularPosition(theta, phi);
	}

	/**
	 * renders the pixel number ipix (RING scheme) for a pixel which contains a
	 * point on a sphere at coordinates theta and phi, given the map resolution
	 * parametr nside the computation is made to the highest resolution
	 * available (nside=8192) and then degraded to that required (by integer
	 * division) this doesn't cost more, and it makes sure that the treatement
	 * of round-off will be consistent for every resolution
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return pixel index in ring scheme
	 * @throws Exception
	 */
	public static final int ang2pix_ring(int nside, double theta, double phi)
			throws Exception {

		double z0 = 0.66666666666666666666666; // 2/3
		int ipix;

		int nl2, nl4, ncap, npix, jp, jm, ipix1;
		double z, za, tt, tp, tmp;
		int ir, ip, kshift;

		// -----------------------------------------------------------------------
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		if (theta < 0.0 || theta > Constants.PI)
			throw new Exception("theta out of range");

		z = Math.cos(theta);
		za = Math.abs(z);
		if (phi >= Constants.twopi)
			phi = phi - Constants.twopi;
		if (phi < 0.)
			phi = phi + Constants.twopi;
		tt = phi / Constants.piover2;// in [0,4)

		nl2 = 2 * nside;
		nl4 = 4 * nside;
		ncap = nl2 * (nside - 1);// number of pixels in the north polar cap
		npix = (int) (12 * Math.pow(nside, 2));

		// equatorial
		if (za <= z0) {

			jp = (int) (nside * (0.50 + tt - z * 0.750));// index of
			// ascending edge
			// line
			jm = (int) (nside * (0.50 + tt + z * 0.750));// index of
			// descending edge
			// line

			ir = nside + 1 + jp - jm;// in {1,2n+1} (ring number counted from
			// z=2/3)
			kshift = 0;
			if (ir % 2 == 0)
				kshift = 1;// kshift=1 if ir even, 0 otherwise

			ip = (int) ((jp + jm - nside + kshift + 1) / 2) + 1;// in {1,4n}
			if (ip > nl4)
				ip = ip - nl4;

			ipix1 = ncap + nl4 * (ir - 1) + ip;

		} else { // caps

			tp = tt - (int) (tt);// MOD(tt,1.0)
			tmp = nside * Math.sqrt(3.0 * (1.0 - za));

			jp = (int) (tp * tmp);// increasing edge line index
			jm = (int) ((1.0 - tp) * tmp);// decreasing edge line index

			ir = jp + jm + 1;// ring number counted from the closest pole
			ip = (int) (tt * ir) + 1;// in {1,4*ir}
			if (ip > 4 * ir)
				ip = ip - 4 * ir;

			ipix1 = 2 * ir * (ir - 1) + ip;
			if (z <= 0.0) {
				ipix1 = npix - 2 * ir * (ir + 1) + ip;
			}
			;

		}
		;

		ipix = ipix1 - 1;// in {0, npix-1}

		return ipix;

	}

	/**
	 * performs conversion from NESTED to RING pixel number
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipnest
	 *            index in nested scheme
	 * @return index in ring scheme
	 * @throws Exception
	 */
	public static final int nest2ring(int nside, int ipnest) throws Exception {

		int ipring;

		int npix, npface, face_num, ncap, n_before;
		int ipf, ip_low, ip_trunc, ip_med, ip_hi;
		int ix, iy, jrt, jr, nr, jpt, jp, kshift, nl4;

		// coordinate of the lowest corner of each face
		// 0 added in front because the java array is zero offset
		int jrll[] = { 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 };
		int jpll[] = { 0, 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 };
		// -----------------------------------------------------------------------
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		npix = (int) (12 * Math.pow(nside, 2));
		if (ipnest < 0 || ipnest > npix - 1)
			throw new Exception("ipnest out of range");

		// initiates the array for the pixel number -> (x,y) mapping
		if (pix2x[1023] <= 0)
			mkpix2xy();

		ncap = 2 * nside * (nside - 1);// number of points in the North Polar
		// cap
		nl4 = 4 * nside;

		// finds the face, and the number in the face
		npface = (int) Math.pow(nside, 2);

		face_num = ipnest / npface;// face number in {0,11}
		ipf = ipnest % npface;// pixel number in the face {0,npface-1}

		// finds the x,y on the face (starting from the lowest corner)
		// from the pixel number
		ip_low = ipf % 1024;// content of the last 10 bits
		ip_trunc = ipf / 1024;// truncation of the last 10 bits
		ip_med = ip_trunc % 1024;// content of the next 10 bits
		ip_hi = ip_trunc / 1024;// content of the high weight 10 bits

		ix = 1024 * pix2x[ip_hi] + 32 * pix2x[ip_med] + pix2x[ip_low];
		iy = 1024 * pix2y[ip_hi] + 32 * pix2y[ip_med] + pix2y[ip_low];

		// transforms this in (horizontal, vertical) coordinates
		jrt = ix + iy;// 'vertical' in {0,2*(nside-1)}
		jpt = ix - iy;// 'horizontal' in {-nside+1,nside-1}

		// computes the z coordinate on the sphere
		jr = jrll[face_num + 1] * nside - jrt - 1;// ring number in
		// {1,4*nside-1}

		nr = nside;// equatorial region (the most frequent)
		n_before = ncap + nl4 * (jr - nside);
		kshift = (jr - nside) % 2;
		if (jr < nside) { // north pole region
			nr = jr;
			n_before = 2 * nr * (nr - 1);
			kshift = 0;
		} else {
			if (jr > 3 * nside) { // south pole region
				nr = nl4 - jr;
				n_before = npix - 2 * (nr + 1) * nr;
				kshift = 0;
			}
		}
		;

		// computes the phi coordinate on the sphere, in [0,2Pi]
		jp = (jpll[face_num + 1] * nr + jpt + 1 + kshift) / 2;// 'phi' number
		// in the ring
		// in {1,4*nr}

		if (jp > nl4)
			jp = jp - nl4;
		if (jp < 1)
			jp = jp + nl4;

		ipring = n_before + jp - 1;// in {0, npix-1}
		return ipring;
	}

	/**
	 * performs conversion from RING to NESTED pixel number
	 * 
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipring
	 *            index pixel in ring scheme
	 * @return index in nest scheme
	 * @throws Exception
	 */
	public static final int ring2nest(int nside, int ipring) throws Exception {
		int ipnest;

		double fihip, hip;
		int npix, nl2, nl4, ncap, ip, iphi, ipt, ipring1;
		int kshift, face_num = 0, nr;
		int irn, ire, irm, irs, irt, ifm, ifp;
		int ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf;

		// coordinate of the lowest corner of each face
		// 0 added in front because the java array is zero offset
		int jrll[] = { 0, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 };
		int jpll[] = { 0, 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 };
		// -----------------------------------------------------------------------
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		npix = (int) (12 * Math.pow(nside, 2));// total number of points
		if (ipring < 0 || ipring > npix - 1)
			throw new Exception("ipring out of range");
		if (x2pix[127] <= 0)
			mkxy2pix();

		nl2 = 2 * nside;
		nl4 = 4 * nside;
		ncap = 2 * nside * (nside - 1);// points in each polar cap, =0 for
		// nside =1
		ipring1 = ipring + 1;

		// finds the ring number, the position of the ring and the face number
		if (ipring1 <= ncap) { // North Pole
			hip = ipring1 / 2.0;
			fihip = Math.rint(hip);
			irn = (int) Math.floor(Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
			// from
			// North
			// pole
			iphi = ipring1 - 2 * irn * (irn - 1);
			kshift = 0;
			nr = irn;// 1/4 of the number of points on the current ring
			face_num = (iphi - 1) / irn;// in {0,3}

		} else {
			if (ipring1 <= nl2 * (5 * nside + 1)) { // Equatorial

				ip = ipring1 - ncap - 1;
				irn = (int) Math.floor(ip / nl4) + nside;// counted from
				// North pole
				iphi = ip % nl4 + 1;

				kshift = (irn + nside) % 2; // 1 if irn+nside is odd, 0
				// otherwise
				nr = nside;
				ire = irn - nside + 1;// in {1, 2*nside +1}
				irm = nl2 + 2 - ire;
				ifm = (iphi - ire / 2 + nside - 1) / nside;// face boundary
				ifp = (iphi - irm / 2 + nside - 1) / nside;
				if (ifp == ifm) { // faces 4 to 7
					face_num = ifp % 4 + 4;
				} else {
					if (ifp + 1 == ifm) { // (half-)faces 0 to 3
						face_num = ifp;
					} else {
						if (ifp - 1 == ifm) { // (half-)faces 8 to 11
							face_num = ifp + 7;
						}
					}
					;
				}

			} else { // South

				ip = npix - ipring1 + 1;
				hip = ip / 2.0;
				fihip = Math.rint(hip);
				irs = (int) Math.floor(Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
				// from
				// South
				// pole
				iphi = 4 * irs + 1 - (ip - 2 * irs * (irs - 1));

				kshift = 0;
				nr = irs;
				irn = nl4 - irs;
				face_num = (iphi - 1) / irs + 8;// in {8,11}

			}
		}
		;

		// finds the (x,y) on the face
		irt = irn - jrll[face_num + 1] * nside + 1;// in {-nside+1,0}
		ipt = 2 * iphi - jpll[face_num + 1] * nr - kshift - 1;// in
		// {-nside+1,nside-1}
		if (ipt >= nl2)
			ipt = ipt - 8 * nside;// for the face #4

		ix = (ipt - irt) / 2;
		iy = -(ipt + irt) / 2;

		// System.out.println("face:"+face_num+" irt:"+irt+" ipt:"+ipt+"
		// ix:"+ix+" iy:"+iy);
		ix_low = ix % 128;
		ix_hi = ix / 128;
		iy_low = iy % 128;
		iy_hi = iy / 128;

		ipf = (x2pix[ix_hi] + y2pix[iy_hi]) * (128 * 128)
				+ (x2pix[ix_low] + y2pix[iy_low]);// in {0, Math.pow(nside,2)
		// - 1}

		ipnest = (int) (ipf + face_num * Math.pow(nside, 2));// in {0,
		// 12*Math.pow(nside,2)
		// - 1}

		return ipnest;
	}

	/**
	 * Convert from pix number to x,y inside a given face. 0,0 is the lower
	 * right corner of the face.
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipix
	 *            index pixel nested
	 * @return x,y {@link Point}
	 * @throws Exception
	 */
	public static final Point pix2xy_nest(int nside, int ipix) throws Exception {
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		if (ipix < 0 || ipix > nside * nside - 1)
			throw new Exception("ipix out of range");
		if (pix2x[1023] <= 0)
			mkpix2xy();
		return pix2xy_nest(ipix);

	}

	/**
	 * Convert from a x,y in a given face to a pix number.
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ix
	 *            x poisiton
	 * @param iy
	 *            y position
	 * @param face
	 *            face number
	 * @return index pixel nested
	 * @throws Exception
	 */
	public static final int xy2pix_nest(int nside, int ix, int iy, int face)
			throws Exception {
		if (nside < 1 || nside > ns_max)
			throw new Exception("nside out of range");
		if (ix < 0 || ix > (nside - 1))
			throw new Exception("ix out of range");
		if (iy < 0 || iy > (nside - 1))
			throw new Exception("iy out of range");

		int ipf, ipix;
		ipf = xy2pix_nest(ix, iy);
		ipix = (int) (ipf + face * Math.pow(nside, 2));// in {0,
		// 12*(nside^2)-1}
		return ipix;
	}

	/**
	 * Convert from a point in a given face to a pix number. Convenience method
	 * just unpacks the point to x and y and calls the other xy2pix_nest method.
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param p
	 *            x,y point object
	 * @param face
	 *            face number
	 * @return index pixel nested
	 * @throws Exception
	 */
	public static final int xy2pix_nest(int nside, Point p, int face)
			throws Exception {
		return xy2pix_nest(nside, p.x, p.y, face);
	}

	/**
	 * Convert from a x,y in a given face to a pix number in a face without
	 * offset.
	 * 
	 * @param ix
	 *            x position
	 * @param iy
	 *            y position
	 * @return index pixel nested
	 * @throws Exception
	 */
	public static final int xy2pix_nest(int ix, int iy) throws Exception {
		int ix_low, ix_hi, iy_low, iy_hi, ipf;
		if (x2pix[127] <= 0)
			mkxy2pix();

		ix_low = ix % 128;
		ix_hi = ix / 128;
		iy_low = iy % 128;
		iy_hi = iy / 128;

		ipf = (x2pix[ix_hi] + y2pix[iy_hi]) * (128 * 128)
				+ (x2pix[ix_low] + y2pix[iy_low]);
		return ipf;
	}

	/**
	 * Convert from pix number to x,y inside a given face. 0,0 is the lower
	 * right corner of the face.
	 * 
	 * @param ipix
	 *            index nested scheme
	 * @return x,y {@link Point}
	 * @throws Exception
	 */
	public static final Point pix2xy_nest(int ipix) throws Exception {
		if (pix2x[1023] <= 0)
			mkpix2xy();
		int ip_low, ip_trunc, ip_med, ip_hi, ix, iy;
		ip_low = ipix % 1024;// content of the last 10 bits
		ip_trunc = ipix / 1024;// truncation of the last 10 bits
		ip_med = ip_trunc % 1024;// content of the next 10 bits
		ip_hi = ip_trunc / 1024;// content of the high weight 10 bits
		ix = 1024 * pix2x[ip_hi] + 32 * pix2x[ip_med] + pix2x[ip_low];
		iy = 1024 * pix2y[ip_hi] + 32 * pix2y[ip_med] + pix2y[ip_low];
		// System.out.println("ip_low:"+ip_low+" iptrun:"+ip_trunc+"
		// ip_med"+ip_med+" ip_hi"+ip_hi+" ix:"+ix+" iy:"+iy);
		return new Point(ix, iy);
	}

	/**
	 * integration limits in cos(theta) for a given ring i_th, i_th > 0
	 * 
	 * @param n_side
	 *            resolution parameter
	 * @param i_th
	 *            ith ring
	 * @return limits
	 */
	public static final double[] integration_limits_in_costh(int n_side,
			int i_th) {

		double a, ab, b, r_n_side;

		// integration limits in cos(theta) for a given ring i_th
		// i > 0 !!!

		r_n_side = 1.0 * n_side;
		if (i_th <= n_side) {
			ab = 1.0 - (Math.pow(i_th, 2.0) / 3.0) / Math.pow(r_n_side, 2);
			b = 1.0 - (Math.pow((i_th - 1), 2.0) / 3.0) / Math.pow(r_n_side, 2);
			if (i_th == n_side) {
				a = 2.0 * (n_side - 1.0) / 3.0 / r_n_side;
			} else {
				a = 1.0 - Math.pow((i_th + 1), 2) / 3.0 / Math.pow(r_n_side, 2);
			}
			;

		} else {
			if (i_th < 3 * n_side) {
				ab = 2.0 * (2 * n_side - i_th) / 3.0 / r_n_side;
				b = 2.0 * (2 * n_side - i_th + 1) / 3.0 / r_n_side;
				a = 2.0 * (2 * n_side - i_th - 1) / 3.0 / r_n_side;
			} else {
				if (i_th == 3 * n_side) {
					b = 2.0 * (-n_side + 1) / 3.0 / r_n_side;
				} else {
					b = -1.0 + Math.pow((4 * n_side - i_th + 1), 2) / 3.0
							/ Math.pow(r_n_side, 2);
				}

				a = -1.0 + Math.pow((4 * n_side - i_th - 1), 2) / 3.0
						/ Math.pow(r_n_side, 2);
				ab = -1.0 + Math.pow((4 * n_side - i_th), 2) / 3.0
						/ Math.pow(r_n_side, 2);
			}

		}
		// END integration limits in cos(theta)
		double[] ret = { b, ab, a };
		return ret;
	}

	/**
	 * calculate the points of crossing for a given theta on the boundaries of
	 * the pixel - returns the left and right phi crossings
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param i_th
	 *            ith pixel
	 * @param i_phi
	 *            phi angle
	 * @param i_zone
	 *            ith zone (0,...,3), a quarter of sphere
	 * @param cos_theta
	 *            theta cosinus
	 * @return the left and right phi crossings
	 */
	public static final double[] pixel_boundaries(int nside, double i_th,
			double i_phi, int i_zone, double cos_theta) {
		double sq3th, factor, jd, ju, ku, kd, phi_l, phi_r;
		double r_n_side = 1.0 * nside;

		// HALF a pixel away from both poles
		if (Math.abs(cos_theta) >= 1.0 - 1.0 / 3.0 / Math.pow(r_n_side, 2)) {
			phi_l = i_zone * Constants.piover2;
			phi_r = (i_zone + 1) * Constants.piover2;
			double[] ret = { phi_l, phi_r };
			return ret;
		}
		// -------
		// NORTH POLAR CAP
		if (1.50 * cos_theta >= 1.0) {
			sq3th = Math.sqrt(3.0 * (1.0 - cos_theta));
			factor = 1.0 / r_n_side / sq3th;
			jd = (double) (i_phi);
			ju = jd - 1;
			ku = (double) (i_th - i_phi);
			kd = ku + 1;
			// System.out.println(" cos_theta:"+cos_theta+" sq3th:"+sq3th+"
			// factor:"+factor+" jd:"+jd+" ju:"+ju+" ku:"+ku+" kd:"+kd+ "
			// izone:"+i_zone);
			phi_l = Constants.piover2
					* (Math.max((ju * factor), (1.0 - (kd * factor))) + i_zone);
			phi_r = Constants.piover2
					* (Math.min((1.0 - (ku * factor)), (jd * factor)) + i_zone);

		} else {
			if (-1.0 < 1.50 * cos_theta) {
				// -------
				// -------
				// EQUATORIAL ZONE
				double cth34 = 0.50 * (1.0 - 1.50 * cos_theta);
				double cth34_1 = cth34 + 1.0;
				int modfactor = (int) (nside + (i_th % 2));

				jd = i_phi - (modfactor - i_th) / 2.0;
				ju = jd - 1;
				ku = (modfactor + i_th) / 2.0 - i_phi;
				kd = ku + 1;

				phi_l = Constants.piover2
						* (Math.max((cth34_1 - (kd / r_n_side)),
								(-cth34 + (ju / r_n_side))) + i_zone);

				phi_r = Constants.piover2
						* (Math.min((cth34_1 - (ku / r_n_side)),
								(-cth34 + (jd / r_n_side))) + i_zone);
				// -------
				// -------
				// SOUTH POLAR CAP

			} else {
				sq3th = Math.sqrt(3.0 * (1.0 + cos_theta));
				factor = 1.0 / r_n_side / sq3th;
				int ns2 = 2 * nside;

				jd = i_th - ns2 + i_phi;
				ju = jd - 1;
				ku = ns2 - i_phi;
				kd = ku + 1;

				phi_l = Constants.piover2
						* (Math.max((1.0 - (ns2 - ju) * factor),
								((ns2 - kd) * factor)) + i_zone);

				phi_r = Constants.piover2
						* (Math.min((1.0 - (ns2 - jd) * factor),
								((ns2 - ku) * factor)) + i_zone);
			}// of SOUTH POLAR CAP
		}
		// and that's it

		double[] ret = { phi_l, phi_r };
		return ret;

	}

	/**
	 * return ring number for given pix in ring scheme
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param ipix
	 *            index pixel in ring scheme
	 * @return ring number
	 * @throws Exception
	 */
	public static final int ring(int nside, int ipix) throws Exception {
		int iring = 0;
		int ipix1 = ipix + 1;// in {1, npix}
		int nl2 = 2 * nside;
		int nl4 = 4 * nside;
		int npix = (int) (12 * (Math.pow(nside, 2)));
		int ncap = 2 * nside * (nside - 1);// points in each polar cap, =0 for
		// nside =1
		int ip;
		double hip, fihip = 0;
		if (ipix1 <= ncap) { // North Polar cap -------------
			hip = ipix1 / 2.0;
			fihip = (int) (hip);
			iring = (int) (Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
			// from
			// North
			// pole
		} else {
			if (ipix1 <= nl2 * (5 * nside + 1)) { // Equatorial region ------
				ip = ipix1 - ncap - 1;
				iring = (int) (ip / nl4) + nside;// counted from North pole
			} else { // South Polar cap -----------------------------------
				ip = npix - ipix1 + 1;
				hip = ip / 2.0;
				fihip = (int) (hip);
				iring = (int) (Math.sqrt(hip - Math.sqrt(fihip))) + 1;// counted
				// from
				// South
				// pole
				iring = nl4 - iring;
			}
		}
		;
		return iring;
	}

	/**
	 * returns the list of pixels in RING or NESTED scheme (listir) with
	 * latitude in [phi0-dphi, phi0+dphi] on the ring ir (in {1,4*nside-1}) the
	 * pixel id-numbers are in {0,12*nside^2-1} the indexing is RING, unless
	 * NEST is set to true
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param iz
	 *            ring number
	 * @param phi0
	 *            phi angle
	 * @param dphi
	 *            delta phi
	 * @param nest
	 *            scheme flag: nest scheme if true, ring scheme otherwise
	 * @return list of pxiel indexes in ring
	 * @throws Exception
	 */
	public int[] in_ring(int nside, int iz, double phi0, double dphi,
			boolean nest) throws Exception {
		int nir;
		int[] listir;

		boolean conservative = false;
		boolean take_all, to_top, do_ring;

		int ip_low, ip_hi, i, diff;
		int npix, nr, nir1, nir2, ir, ipix1, ipix2, kshift, ncap;
		double phi_low, phi_hi, shift;
		// =======================================================================;

		take_all = false;
		to_top = false;
		do_ring = !nest;

		npix = 12 * nside * nside;
		ncap = 2 * nside * (nside - 1);// number of pixels in the north polar
		// cap;
		nir = 0;

		phi_low = (phi0 - dphi) % (Constants.twopi);
		phi_hi = (phi0 + dphi) % (Constants.twopi);
		if (Math.abs(dphi - Constants.PI) < 1.0)
			take_all = true;

		// ------------ identifies ring number --------------;
		if (iz >= nside && iz <= 3 * nside) { // equatorial region
			ir = iz - nside + 1; // in {1, 2*nside + 1}
			ipix1 = ncap + 4 * nside * (ir - 1);// lowest pixel number in the
			// ring;
			ipix2 = ipix1 + 4 * nside - 1; // highest pixel number in the ring;
			kshift = (ir) % (2);
			nr = nside * 4;
		} else {
			if (iz < nside) { // north pole
				ir = iz;
				ipix1 = 2 * ir * (ir - 1); // lowest pixel number in the ring;
				ipix2 = ipix1 + 4 * ir - 1; // highest pixel number in the ring;
			} else {
				; // south pole
				ir = 4 * nside - iz;
				ipix1 = npix - 2 * ir * (ir + 1);// lowest pixel number in
				// the ring;
				ipix2 = ipix1 + 4 * ir - 1; // highest pixel number in the ring;
			}
			nr = ir * 4;
			kshift = 1;
		}

		// ----------- constructs the pixel list --------------;
		if (take_all) {
			nir = ipix2 - ipix1 + 1;
			listir = new int[nir];
			if (do_ring) {
				for (i = 0; i < nir; i++) {
					listir[i] = ipix1 + i;
				}

			} else {
				for (i = 0; i < nir; i++) {
					listir[i] = ring2nest(nside, ipix1 + i);
				}
			}
			return listir;
		}

		shift = kshift * 0.5;
		if (conservative) {
			// conservative : include every intersected pixels,;
			// even if pixel CENTER is not in the range [phi_low, phi_hi];
			ip_low = (int) (nr * phi_low / Constants.twopi - shift);
			ip_hi = (int) (nr * phi_hi / Constants.twopi - shift);
			ip_low = ip_low % nr; // in {0,nr-1}
			ip_hi = ip_hi % nr; // in {0,nr-1}
		} else {
			// strict : include only pixels whose CENTER is in [phi_low,
			// phi_hi];
			ip_low = (int) Math.ceil(nr * phi_low / Constants.twopi - shift);
			ip_hi = (int) Math.floor(nr * phi_hi / Constants.twopi - shift);
			diff = (ip_low - ip_hi) % nr; // in {-nr+1, nr-1} or {0,nr-1} ???
			if (diff < 0)
				diff = diff + nr; // in {0,nr-1}
			if ((diff == 1) && (dphi * nr < Constants.PI)) {
				// the interval is so small (and away from pixel center);
				// that no pixel is included in it;
				nir = 0;
				return new int[nir];
			}
			if (ip_low >= nr)
				ip_low = ip_low - nr;
			if (ip_hi < 0)
				ip_hi = ip_hi + nr;
		}
		// ;
		if (ip_low > ip_hi)
			to_top = true;
		ip_low = ip_low + ipix1;
		ip_hi = ip_hi + ipix1;

		if (to_top) {
			nir1 = ipix2 - ip_low + 1;
			nir2 = ip_hi - ipix1 + 1;
			nir = nir1 + nir2;
			listir = new int[nir];
			if (do_ring) {
				for (i = 0; i < nir1; i++) {
					listir[i] = ip_low + i;
				}
				int ic = 0;
				for (i = nir1; i < nir; i++) {
					listir[i] = ipix1 + ic++;
				}

			} else {
				for (i = 0; i < nir1; i++) {
					listir[i] = ring2nest(nside, ip_low + i);
				}
				int ic = 0;
				for (i = nir1; i < nir; i++) {
					listir[i] = ring2nest(nside, ipix1 + ic++);
				}
			}
			return listir;

		} else {
			nir = ip_hi - ip_low + 1;
			listir = new int[nir];
			if (do_ring) {
				for (i = 0; i < nir; i++) {
					listir[i] = ip_low + i;
				}

			} else {
				for (i = 0; i < nir; i++) {
					listir[i] = ring2nest(nside, ip_low + i);
				}
			}
			return listir;

		}

	}// in_ring

	/**
	 * 
	 * query_disc (Nside, Vector0, Radius, Listpix, Nlist[, Nest, Inclusive]).
	 * routine for pixel query in the RING or NESTED scheme. All pixels within
	 * an angular distance Radius of the center.
	 * 
	 * Nside = resolution parameter (a power of 2); Vector0 = central point
	 * vector position (x,y,z in double precision); Radius = angular radius in
	 * RADIAN (in double precision); Listpix = list of pixel closer to the
	 * center (angular distance) than Radius; Nlist = number of pixels in the
	 * list; nest (OPT), :0 by default, the output list is in RING scheme; if
	 * set to 1, the output list is in NESTED scheme; inclusive (OPT) , :0 by
	 * default, only the pixels whose center; lie in the triangle are listed on
	 * output; if set to 1, all pixels overlapping the triangle are output;
	 * 
	 * all pixel numbers are in {0, 12*Nside*Nside - 1} NB : the dimension of
	 * the listpix array is fixed in the ing; routine and should be large enough
	 * for the specific configuration.
	 * 
	 * lower level public void s ed by getdisc_ring : (you don't need to know
	 * them); ring_num (nside, ir); in_ring(nside, iz, phi0, dphi, listir, nir,
	 * nest=nest);
	 * 
	 * @param nside
	 *            resolution parameter
	 * @param vector0
	 *            {@link SpatialVector} poitnig to disc
	 * @param radius
	 *            disc radius
	 * @param nest
	 *            scheme nest if true
	 * @param inclusive
	 *            include disc flag
	 * @return list of index pixel contained in disc
	 * @throws Exception
	 */
	public int[] query_disc(int nside, SpatialVector vector0, double radius,
			boolean nest, boolean inclusive) throws Exception {

		int irmin, irmax, ilist, iz, ip, nir = 0;
		double norm_vect0;
		double x0, y0, z0, radius_eff;
		double a, b, c, cosang;
		double dth1, dth2;
		double phi0, cosphi0, cosdphi, dphi = 0;
		double rlat0, rlat1, rlat2, zmin, zmax, z;
		int[] listir;
		// String code = "QUERY_DISC";
		int list_size = 0, nlost;
		boolean skip = false;
		ArrayList<Integer> listpix = new ArrayList<Integer>();

		// =======================================================================;

		// ---------- check inputs ----------------;

		if (radius < 0.0 || radius > Constants.PI) {
			throw new Exception("Radius should be between 0 and Pi. is "
					+ radius);
		}

		// --------- allocate memory -------------;
		listir = new int[4 * nside - 1];

		dth1 = 1.0 / (3.0 * (double) Math.pow(nside, 2));
		dth2 = 2.0 / (3.0 * (double) (nside));

		radius_eff = radius;
		if (inclusive) {
			// increase radius by half pixel size;
			radius_eff = radius + Constants.magic* Constants.PI / (4.0 * nside);
		}
		cosang = Math.cos(radius_eff);

		// ---------- circle center -------------;
		norm_vect0 = Math.sqrt(vector0.dot(vector0));
		x0 = vector0.x() / norm_vect0;
		y0 = vector0.y() / norm_vect0;
		z0 = vector0.z() / norm_vect0;

		phi0 = 0.0;
		if ((x0 == 0.0) || (y0 == 0.0))
			phi0 = Math.atan2(y0, x0); // in ]-Pi, Pi];
		cosphi0 = Math.cos(phi0);
		a = x0 * x0 + y0 * y0;

		// --- coordinate z of highest and lowest points in the disc ---;
		rlat0 = Math.asin(z0); // latitude in RAD of the center;
		rlat1 = rlat0 + radius_eff;
		rlat2 = rlat0 - radius_eff;
		if (rlat1 >= Constants.piover2) {
			zmax = 1.0;
		} else {
			zmax = Math.sin(rlat1);
		}
		irmin = ring_num(nside, zmax);
		irmin = Math.max(1, irmin - 1);// start from a higher point, to be
		// safe;

		if (rlat2 <= -Constants.piover2) {
			zmin = -1.0;
		} else {
			zmin = Math.sin(rlat2);
		}
		irmax = ring_num(nside, zmin);
		irmax = Math.min(4 * nside - 1, irmax + 1);// go down to a lower point;

		ilist = -1;

		// ------------- loop on ring number ---------------------;
		for (iz = irmin; iz < irmax; iz++) {
			if (iz <= nside - 1) { // north polar cap
				z = 1.0 - Math.pow(iz, 2) * dth1;
			} else {
				if (iz <= 3 * nside) { // tropical band + equat.
					z = ((double) (2 * nside - iz)) * dth2;
				} else {
					z = -1.0 + Math.pow(4 * nside - iz, 2) * dth1;
				}

				// --------- phi range in the disc for each z ---------;
				b = cosang - z * z0;
				c = 1.0 - z * z;
				if ((x0 == 0.0) && (y0 == 0.0)) {
					cosdphi = -1.0;
					dphi = Constants.PI;
				} else {
					cosdphi = b / Math.sqrt(a * c);
					if (Math.abs(cosdphi) <= 1.0) {
						dphi = Math.acos(cosdphi);// in [0,Pi];
					} else {
						if (cosphi0 < cosdphi) {
							skip = true;// out of the disc;
						} else {
							dphi = Constants.PI;// all the pixels at this elevation are
							// in the disc;
						}
					}
				}
				if (!skip) {
					// ------- finds pixels in the disc ---------;
					listir = in_ring(nside, iz, phi0, dphi, nest);

					// ----------- merge pixel lists -----------;
					nlost = ilist + nir + 1 - list_size;
					if (nlost > 0) {
						// print*,code//"> listpix is too short, it will be
						// truncated at ",nir;
						// print*," pixels lost : ", nlost;
						nir = nir - nlost;
					}
					for (ip = 0; ip < nir - 1; ip++) {
						ilist = ilist + 1;
						listpix.add(new Integer(listir[ip]));
					}// enddo
				} else {
					skip = false;
				}// end if skip
			}// enddo (1000)
		}// end query_disc;
		int[] rlist = new int[listpix.size()];
		for (int p = 0; p < rlist.length; p++) {
			rlist[p] = ((Integer) listpix.get(p)).intValue();
		}
		return rlist;
	}

	/**
	 * returns the ring number in {1, 4*nside-1} from the z coordinate.
	 * 
	 * @param nside
	 *            resoultion parameter
	 * @param z
	 *            z coordinate
	 * @return ring number
	 */
	public int ring_num(int nside, double z) {
		int iring;

		// ----- equatorial regime ---------;
		iring = (int) (Math.round(nside * (2.0 - 1.500 * z)));

		// ----- north cap ------;
		if (z > Constants.twothird) {
			iring = (int) Math.round(nside * Math.sqrt(3.0 * (1.0 - z)));
			if (iring == 0)
				iring = 1;
		}

		// ----- south cap -----;
		if (z < -Constants.twothird) {
			iring = (int) Math.round(nside * Math.sqrt(3.0 * (1.0 + z)));
			if (iring == 0)
				iring = 1;
			iring = 4 * nside - iring;
		}

		return iring;
	}

}
