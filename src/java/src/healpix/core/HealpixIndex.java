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
 *  For more information about HEALPix, see http://healpix.jpl.nasa.gov

 *
 */
package healpix.core;

import healpix.core.base.set.LongRangeSet;
import healpix.tools.Constants;
import healpix.tools.SpatialVector;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Generic healpix routines but tied to a given NSIDE in the constructor Java
 * version of some healpix routines from DSRI in java everything must be in a
 * class - no functions floating about. Original algorithms Eric Hivon and
 * Krzysztof M. Gorski. This code written by William O'Mullane extended by
 * Emmanuel Joliet with some methods added from pix_tools F90 code port to Java.
 * Performance for 64bits resolution improved using code from Jan Kotek and
 * inspired in PCJ (http://pcj.sourceforge.net/)
 * 
 * @author William O'Mullane, extended by Emmanuel Joliet
 * @version $Id: HealpixIndex.java 164005 2011-01-10 11:10:33Z ejoliet $
 */

public class HealpixIndex extends HealpixBase implements Serializable {
	
	/**
	 * Default serial version
	 */
	private static final long serialVersionUID = 2L;
    /**
     * the actual version  from SVN
     */
    public static final String REVISION =
        "$Id: HealpixIndex.java 164005 2011-01-10 11:10:33Z ejoliet $";

	/**
	 * Default constructor nside = 1024.
	 */
	public HealpixIndex() {
		try {
		setNsideAndScheme(1024,Scheme.NESTED);
                }
		catch (Exception Ex) { /* cannot happen */ }
	}

	/**
	 * Construct healpix routines tied to a given nside
	 * 
	 * @param nSIDE2
	 *            resolution number
	 * @throws Exception
	 */
	public HealpixIndex(int nSIDE2) throws Exception {
		super (nSIDE2, Scheme.NESTED);
	}

	public HealpixIndex(int nside2, Scheme sch) throws Exception{
		super (nside2, sch);
	}

	/**
	 * renders the pixel number ipix ( scheme as defined for object) 
	 * for a pixel which contains
	 * a point on a sphere at coordinates theta and phi, given the map
	 * resolution parameter nside 
	 * 
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return pixel index number
	 * @throws Exception
	 */
	public long ang2pix_nest(double theta, double phi) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.NESTED);
		long result=super.ang2pix(new Pointing(theta,phi));
		setScheme(sbak);
		return result;
	}

	/**
	 * Convert from pix number to angle renders theta and phi coordinates of the
	 * nominal pixel center for the pixel number ipix (NESTED scheme) given the
	 * map resolution parameter nside
	 * 
	 * @param ipix
	 *            pixel index number
	 * @return double array of [theta, phi] angles in radians [0,Pi], [0,2*Pi]
	 * @throws Exception
	 */
	public double[] pix2ang_nest(long ipix) throws Exception {
                Scheme sbak=scheme;
		setScheme(Scheme.NESTED);
                Pointing res = super.pix2ang(ipix);
		setScheme(sbak);
		double[] ret = { res.theta, res.phi };
		return ret;
	}

	/**
	 * Convert from pix number to angle renders theta and phi coordinates of the
	 * nominal pixel center for the pixel number ipix (RING scheme) given the
	 * map resolution parameter nside
	 * 
	 * @param ipix
	 *            pixel index number
	 * @return double array of [theta, phi] angles in radians [0,Pi], [0,2*Pi]
	 * @throws Exception
	 */
	public double[] pix2ang_ring(long ipix) throws Exception {
                Scheme sbak=scheme;
		setScheme(Scheme.RING);
                Pointing res = super.pix2ang(ipix);
		setScheme(sbak);
		double[] ret = { res.theta, res.phi };
		return ret;
	}

	/**
	 * renders the pixel number ipix (RING scheme) for a pixel which contains a
	 * point on a sphere at coordinates theta and phi, given the map resolution
	 * parameter nside the computation is made to the highest resolution
	 * available (nside=8192) and then degraded to that required (by integer
	 * division) this doesn't cost more, and it makes sure that the treatement
	 * of round-off will be consistent for every resolution
	 * 
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return pixel index number
	 * @throws Exception
	 */
	public long ang2pix_ring(double theta, double phi) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.RING);
		long result=super.ang2pix(new Pointing(theta,phi));
		setScheme(sbak);
		return result;
	}

	/**
	 * integration limits in cos(theta) for a given ring i_th, i_th > 0
	 * 
	 * @param i_th
	 *            ith ring
	 * @return limits
	 */
	public double[] integration_limits_in_costh(int i_th) {

		double a, ab, b, r_n_side;

		// integration limits in cos(theta) for a given ring i_th
		// i > 0 !!!

		r_n_side = 1.0 * (long)nside;
		if ( i_th <= (long)nside ) {
			ab = 1.0 - ( Math.pow(i_th, 2.0) / 3.0 ) / (double) npface;
			b = 1.0 - ( Math.pow(( i_th - 1 ), 2.0) / 3.0 ) / (double) npface;
			if ( i_th == (long)nside ) {
				a = 2.0 * ( (long)nside - 1.0 ) / 3.0 / r_n_side;
			} else {
				a = 1.0 - Math.pow(( i_th + 1 ), 2) / 3.0 / (double) npface;
			};

		} else {
			if ( i_th < nl3 ) {
				ab = 2.0 * ( 2 * (long)nside - i_th ) / 3.0 / r_n_side;
				b = 2.0 * ( 2 * (long)nside - i_th + 1 ) / 3.0 / r_n_side;
				a = 2.0 * ( 2 * (long)nside - i_th - 1 ) / 3.0 / r_n_side;
			} else {
				if ( i_th == nl3 ) {
					b = 2.0 * ( -(long)nside + 1 ) / 3.0 / r_n_side;
				} else {
					b = -1.0 + Math.pow(( 4 * (long)nside - i_th + 1 ), 2) / 3.0
							/ (double) npface;
				}

				a = -1.0 + Math.pow(( nl4 - i_th - 1 ), 2) / 3.0
						/ (double) npface;
				ab = -1.0 + Math.pow(( nl4 - i_th ), 2) / 3.0 / (double) npface;
			}

		}
		// END integration limits in cos(theta)
		double[] ret = { b, ab, a };
		return ret;
	}

	/**
	 * calculate the points of crossing for a given theata on the boundaries of
	 * the pixel - returns the left and right phi crossings
	 * 
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
	public double[] pixel_boundaries(double i_th, double i_phi, int i_zone,
			double cos_theta) {
		double sq3th, factor, jd, ju, ku, kd, phi_l, phi_r;
		double r_n_side = 1.0 * (long)nside;

		// HALF a pixel away from both poles
		if ( Math.abs(cos_theta) >= 1.0 - 1.0 / 3.0 / (double) npface ) {
			phi_l = i_zone * Constants.piover2;
			phi_r = ( i_zone + 1 ) * Constants.piover2;
			double[] ret = { phi_l, phi_r };
			return ret;
		}
		// -------
		// NORTH POLAR CAP
		if ( 1.50 * cos_theta >= 1.0 ) {
			sq3th = Math.sqrt(3.0 * ( 1.0 - cos_theta ));
			factor = 1.0 / r_n_side / sq3th;
			jd = (double) ( i_phi );
			ju = jd - 1;
			ku = (double) ( i_th - i_phi );
			kd = ku + 1;
			// System.out.println(" cos_theta:"+cos_theta+" sq3th:"+sq3th+"
			// factor:"+factor+" jd:"+jd+" ju:"+ju+" ku:"+ku+" kd:"+kd+ "
			// izone:"+i_zone);
			phi_l = Constants.piover2
					* ( Math.max(( ju * factor ), ( 1.0 - ( kd * factor ) )) + i_zone );
			phi_r = Constants.piover2
					* ( Math.min(( 1.0 - ( ku * factor ) ), ( jd * factor )) + i_zone );

		} else {
			if ( -1.0 < 1.50 * cos_theta ) {
				// -------
				// -------
				// EQUATORIAL ZONE
				double cth34 = 0.50 * ( 1.0 - 1.50 * cos_theta );
				double cth34_1 = cth34 + 1.0;
				int modfactor = (int) ( (long)nside + ( i_th % 2 ) );

				jd = i_phi - ( modfactor - i_th ) / 2.0;
				ju = jd - 1;
				ku = ( modfactor + i_th ) / 2.0 - i_phi;
				kd = ku + 1;

				phi_l = Constants.piover2
						* ( Math.max(( cth34_1 - ( kd / r_n_side ) ),
								( -cth34 + ( ju / r_n_side ) )) + i_zone );

				phi_r = Constants.piover2
						* ( Math.min(( cth34_1 - ( ku / r_n_side ) ),
								( -cth34 + ( jd / r_n_side ) )) + i_zone );
				// -------
				// -------
				// SOUTH POLAR CAP

			} else {
				sq3th = Math.sqrt(3.0 * ( 1.0 + cos_theta ));
				factor = 1.0 / r_n_side / sq3th;
				long ns2 = 2 * (long)nside;

				jd = i_th - ns2 + i_phi;
				ju = jd - 1;
				ku = ns2 - i_phi;
				kd = ku + 1;

				phi_l = Constants.piover2
						* ( Math.max(( 1.0 - ( ns2 - ju ) * factor ),
								( ( ns2 - kd ) * factor )) + i_zone );

				phi_r = Constants.piover2
						* ( Math.min(( 1.0 - ( ns2 - jd ) * factor ),
								( ( ns2 - ku ) * factor )) + i_zone );
			}// of SOUTH POLAR CAP
		}
		// and that's it
		// System.out.println(" nside:"+nside+" i_th:"+i_th+" i_phi:"+i_phi+"
		// izone:"+i_zone+" cos_theta:"+cos_theta+" phi_l:"+phi_l+"
		// phi_r:"+phi_r);

		double[] ret = { phi_l, phi_r };
		return ret;

	}

	/**
	 * return ring number for given pix in ring scheme
	 * 
	 * @param ipix
	 *            pixel index number in ring scheme
	 * @return ring number
	 * @throws Exception
	 */
	public int ring(long ipix) throws Exception {
          if (ipix<ncap) // North Polar cap
            return (int)((1+HealpixUtils.isqrt(1+2*ipix))>>>1); // counted from North pole
          else if (ipix<(npix-ncap)) // Equatorial region
            return (int)((ipix-ncap)/nl4 + nside); // counted from North pole
          else // South Polar cap
            return (int)(nl4-((1+HealpixUtils.isqrt(2*(npix-ipix)-1))>>>1));
	}

	/**
	 * Construct a {@link SpatialVector} from the angle (theta,phi)
	 * 
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return vector {@link SpatialVector}
	 */
	public static SpatialVector vector(double theta, double phi) {
		double x, y, z;
		x = 1 * Math.sin(theta) * Math.cos(phi);
		y = 1 * Math.sin(theta) * Math.sin(phi);
		z = 1 * Math.cos(theta);
		return new SpatialVector(x, y, z);
	}

	/**
	 * Converts the unit vector to pix number in NEST scheme
	 * 
	 * @param vec
	 *            {@link SpatialVector}
	 * @return pixel index number in nest scheme
	 * @throws Exception
	 */
	public long vec2pix_nest(SpatialVector vec) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.NESTED);
		long result=super.vec2pix(vec);
		setScheme(sbak);
		return result;
	}
	/**
	 * Converts the unit vector to pix number in RING scheme
	 * 
	 * @param vec
	 *            {@link SpatialVector}
	 * @return pixel index number in ring scheme
	 * @throws Exception
	 */
	public long vec2pix_ring(SpatialVector vec) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.RING);
		long result=super.vec2pix(vec);
		setScheme(sbak);
		return result;
	}

	/**
	 * Converts pix number in NEST scheme to the unit vector
	 * 
	 * @param pix
	 *            pixel index number in nest scheme
	 * @return {@link SpatialVector}
	 * @throws Exception
	 */
	public SpatialVector pix2vec_nest(long pix) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.NESTED);
		Vec3 res1=super.pix2vec(pix);
		setScheme(sbak);
		return new SpatialVector(res1);
	}

	/**
	 * Converts pix number in RING scheme to the unit vector
	 * 
	 * @param pix
	 *            pixel index number in ring scheme
	 * @return {@link SpatialVector}
	 * @throws Exception
	 */
	public SpatialVector pix2vec_ring(long pix) throws Exception {
		Scheme sbak=scheme;
		setScheme(Scheme.RING);
		Vec3 res1=super.pix2vec(pix);
		setScheme(sbak);
		return new SpatialVector(res1);
	}

	/**
	 * Returns set of points along the boundary of the given pixel in NEST
	 * scheme. Step 1 gives 4 points on the corners.
	 * 
	 * @param pix
	 *            pixel index number in nest scheme
	 * @param step
	 * @return {@link SpatialVector} for each points
	 * @throws Exception
	 */
	public SpatialVector[] corners_nest(int pix, int step) throws Exception {
		long pixr = nest2ring(pix);
		return corners_ring(pixr, step);
	}

	/**
	 * Returns set of points along the boundary of the given pixel in RING
	 * scheme. Step 1 gives 4 points on the corners.
	 * Mainly for graphics = you may not want to use LARGE NSIDEs..
	 * 
	 * @param pix
	 *            pixel index number in ring scheme
	 * @param step
	 * @return {@link SpatialVector} for each points
	 * @throws Exception
	 */
	public SpatialVector[] corners_ring(long pix, int step) throws Exception {
		int nPoints = step * 2 + 2;
		SpatialVector[] points = new SpatialVector[nPoints];
		double[] p0 = pix2ang_ring(pix);
		double cos_theta = Math.cos(p0[0]);
		double theta = p0[0];
		double phi = p0[1];

		int i_zone = (int) ( phi / Constants.piover2 );
		int ringno = ring(pix);
		int i_phi_count = Math.min(ringno, (int)Math.min((long)nside, ( nl4 ) - ringno));
		int i_phi = 0;
		double phifac = Constants.piover2 / i_phi_count;
		if ( ringno >= (long)nside && ringno <= nl3 ) {
			// adjust by 0.5 for odd numbered rings in equatorial since
			// they start out of phase by half phifac.
			i_phi = (int) ( phi / phifac + ( ( ringno % 2 ) / 2.0 ) ) + 1;
		} else {
			i_phi = (int) ( phi / phifac ) + 1;
		}
		// adjust for zone offset
		i_phi = i_phi - ( i_zone * i_phi_count );
		int spoint = (int) ( nPoints / 2 );
		// get north south middle - middle should match theta !
		double[] nms = integration_limits_in_costh(ringno);
		double ntheta = Math.acos(nms[0]);
		double stheta = Math.acos(nms[2]);
		double[] philr = pixel_boundaries(ringno, i_phi, i_zone, nms[0]);
		if ( i_phi > ( i_phi_count / 2 ) ) {
			points[0] = vector(ntheta, philr[1]);
		} else {
			points[0] = vector(ntheta, philr[0]);
		}
		philr = pixel_boundaries(ringno, i_phi, i_zone, nms[2]);
		if ( i_phi > ( i_phi_count / 2 ) ) {
			points[spoint] = vector(stheta, philr[1]);
		} else {
			points[spoint] = vector(stheta, philr[0]);
		}
		if ( step == 1 ) {
			double mtheta = Math.acos(nms[1]);
			philr = pixel_boundaries(ringno, i_phi, i_zone, nms[1]);
			points[1] = vector(mtheta, philr[0]);
			points[3] = vector(mtheta, philr[1]);
		} else {
			double cosThetaLen = nms[2] - nms[0];
			double cosThetaStep = ( cosThetaLen / ( step + 1 ) ); // skip
			// North
			// and south
			for ( int p = 1; p <= step; p++ ) {
				/* Integrate points along the sides */
				cos_theta = nms[0] + ( cosThetaStep * p );
				theta = Math.acos(cos_theta);
				philr = pixel_boundaries(ringno, i_phi, i_zone, cos_theta);
				points[p] = vector(theta, philr[0]);
				points[nPoints - p] = vector(theta, philr[1]);
			}
		}
		return points;
	}

	/**
	 * calculates angular resolution of the pixel map in arc seconds.
	 * 
	 * @param nside
	 * @return double resolution in arcsec
	 */
	static public double getPixRes(long nside) {
		double res = 0.;
		double degrad = Math.toDegrees(1.0);
		double skyArea = 4. * Constants.PI * degrad * degrad; // 4PI steredian
		// in deg^2
		double arcSecArea = skyArea * 3600. * 3600.; // 4PI steredian in
		// (arcSec^2)
		long npixels = 12 * (long)nside * (long)nside;
		res = arcSecArea / npixels; // area per pixel
		res = Math.sqrt(res); // angular size of the pixel arcsec
		return res;
	}

	/**
	 * calculate required nside given pixel size in arcsec
	 * 
	 * @param pixsize
	 *            in arcsec
	 * @return long nside parameter
	 */
	static public int calculateNSide(double pixsize) {
		double pixelArea = pixsize * pixsize;
		double degrad = Math.toDegrees(1.);
		double skyArea = 4. * Constants.PI * degrad * degrad * 3600. * 3600.;
		long npixels = (long) ( skyArea / pixelArea );
		long nsidesq = npixels / 12;
		long order_req = (long)Math.rint(0.5*(Math.log(nsidesq) / Math.log(2)));
                if (order_req<0) order_req=0;
                if (order_req>order_max) {
				System.out.println("nside cannot be bigger than " + ns_max);
				order_req=order_max;
			}
		return 1<<order_req;
	}

	/**
	 * calculates vector corresponding to angles theta (co-latitude measured
	 * from North pole, in [0,pi] radians) phi (longitude measured eastward in
	 * [0,2pi] radians) North pole is (x,y,z) = (0, 0, 1)
	 * 
	 * @param theta
	 *            angle (along meridian), in [0,Pi], theta=0 : north pole
	 * @param phi
	 *            angle (along parallel), in [0,2*Pi]
	 * @return SpatialVector
	 * @throws IllegalArgumentException
	 */
	public static SpatialVector ang2Vec(double theta, double phi) {
		return new SpatialVector(new Vec3 (new Pointing(theta,phi)));
	}

	
	public static AngularPosition vec2AngularPosition(SpatialVector v) {
		return new AngularPosition(new Pointing (v));
	}

	/**
	 * converts a SpatialVector in a tuple of angles tup[0] = theta co-latitude
	 * measured from North pole, in [0,PI] radians, tup[1] = phi longitude
	 * measured eastward, in [0,2PI] radians
	 * 
	 * @param v
	 *            SpatialVector
	 * @return double[] out_tup out_tup[0] = theta out_tup[1] = phi
	 */
	public static double[] vec2Ang(SpatialVector v) {
		Pointing ptg = new Pointing(v);
		double[] out_tup = {ptg.theta,ptg.phi};
		return out_tup;
	}

	/**
	 * calculates the surface of spherical triangle defined by vertices v1,v2,v3
	 * Algorithm: finds triangle sides and uses l'Huilier formula to compute
	 * "spherical excess" = surface area of triangle on a sphere of radius one
	 * see, eg Bronshtein, Semendyayev Eq 2.86 half perimeter hp =
	 * 0.5*(side1+side2+side3) l'Huilier formula x0 = tan( hp/2.) x1 = tan((hp -
	 * side1)/2.) x2 = tan((hp - side2)/2.) x3 = tan((hp - side3)/2.)
	 * 
	 * @param v1
	 *            SpatialVector
	 * @param v2
	 *            SpatialVector
	 * @param v3
	 *            SpatialVector vertices of the triangle
	 * @return double the triangle surface in steradians of the spherical
	 *         triangle with vertices vec1, vec2, vec3
	 * @throws Exception
	 */
	public static double surfaceTriangle(SpatialVector v1, SpatialVector v2,
			SpatialVector v3) throws Exception {
		double res = 0.;
		double side1 = angDist(v2, v3) / 4.0;
		double side2 = angDist(v3, v1) / 4.0;
		double side3 = angDist(v1, v2) / 4.0;
		double x0 = Math.tan(side1 + side2 + side3);
		double x1 = Math.tan(side2 + side3 - side1);
		double x2 = Math.tan(side1 + side3 - side2);
		double x3 = Math.tan(side1 + side2 - side3);
		res = 4.0 * Math.atan(Math.sqrt(x0 * x1 * x2 * x3));

		return res;
	}

	/**
	 * calculates angular distance (in radians) between 2 Vectors v1 and v2.

	 * 
	 * @param v1
	 *            SpatialVector
	 * @param v2
	 *            SpatialVector
	 * @return double dist
	 * @throws Exception
	 */
	public static double angDist(SpatialVector v1, SpatialVector v2) throws Exception {
		return v1.angle(v2);
        }

	
	/**
	 * now using the C++ one this is here for compatibility
	 * @param vec
	 * @param radius
	 * @param nest
	 * @param inclusive
	 * @return a LongRangeSet containing all relevant pixels
	 * @deprecated use oen without nest - scheme now in map
	 */
	public LongRangeSet queryDisc(SpatialVector vec,
			double radius, int nest, int inclusive) throws Exception {
		
		Scheme sbak=scheme;
		setScheme ((nest==0) ? Scheme.RING : Scheme.NESTED);
		LongRangeSet res = super.queryDisc(new Pointing(vec),radius,(inclusive!=0));
		setScheme (sbak);
		return res;
	}
	/**
	 * now using the C++ one this is here for convenience to consruct the LongRangeSet.
	 * It somply calls the queryDisc passing in a LongRangeSetBuilder.
	 * @param vec
	 * @param radius
	 * @param inclusive
	 * @return a LongRangeSet containing all relevant pixels
	 */
	public LongRangeSet queryDisc(SpatialVector vec,
			double radius, boolean inclusive) throws Exception {
		return queryDisc(new Pointing(vec),radius,inclusive);
	}
	

	/**
	 * returns the ring number in {1, 4*nside - 1} calculated from z coordinate
	 * 
	 * @param nside
	 *            long resolution
	 * @param z
	 *            double z coordinate
	 * @return long ring number
	 */
	public static long ringNum(int nside, double z) {
		long iring = 0;
		/* equatorial region */

		iring = (long) Math.round(nside * ( 2.0 - 1.5 * z ));
		/* north cap */
		if ( z > Constants.twothird ) {
			iring = (long) Math.round(nside * Math.sqrt(3.0 * ( 1.0 - z )));
			if ( iring == 0 )
				iring = 1;
		}
		/* south cap */
		if ( z < -Constants.twothird ) {
			iring = (long) Math.round(nside * Math.sqrt(3.0 * ( 1.0 + z )));
			if ( iring == 0 )
				iring = 1;
			iring = 4 * nside - iring;
		}
		return iring;
	}

	/**
	 * finds pixels that lie within a CONVEX polygon defined by its vertex on
	 * sphere
	 * 
	 * @param nside
	 *            the map resolution
	 * @param vlist1
	 *            ArrayList of vectors defining the polygon vertices
	 * @param nest
	 *            if set to 1 use NESTED scheme
	 * @param inclusive
	 *            if set 1 returns all pixels crossed by polygon boundaries
	 * @return ArrayList of pixels algorithm: the polygon is divided into
	 *         triangles vertex 0 belongs to all triangles
	 * @throws Exception
	 */
	public LongRangeSet query_polygon(int nside, ArrayList<SpatialVector> vlist1,
			long nest, long inclusive) throws Exception {
		Scheme sbak=scheme;
		setScheme ((nest==0) ? Scheme.RING : Scheme.NESTED);
		Pointing[] vertex = new Pointing[vlist1.size()];
		for (int i=0; i<vlist1.size(); ++i)
			vertex[i]=new Pointing((Vec3)vlist1.get(i));
		LongRangeSet res = super.queryPolygon(vertex,(inclusive!=0));
		setScheme (sbak);
		return res;
	}
	
	/**
	 * Prints the vec.
	 * 
	 * @param vec the vec
	 */
	public void printVec(double[] vec) {
		System.out.print("[");
		for ( int i = 0; i < vec.length; i++ ) {
			System.out.print(vec[i] + " ");
		}
		System.out.println("]");
	}
	/**
	 * generates a list of pixels that lie inside a triangle defined by the
	 * three vertex vectors
	 * 
	 * @param nside
	 *            long map resolution parameter
	 * @param v1
	 *            Vector3d defines one vertex of the triangle
	 * @param v2
	 *            Vector3d another vertex
	 * @param v3
	 *            Vector3d yet another one
	 * @param nest
	 *            long 0 (default) RING numbering scheme, if set to 1 the NESTED
	 *            scheme will be used.
	 * @param inclusive
	 *            long 0 (default) only pixels whose centers are inside the
	 *            triangle will be listed, if set to 1 all pixels overlaping the
	 *            triangle will be listed
	 * @return ArrayList with pixel numbers
	 * @throws Exception
	 *             if the triangle is degenerated
	 */
	public LongRangeSet query_triangle(int nside, SpatialVector v1,
			SpatialVector v2, SpatialVector v3, long nest, long inclusive)
			throws Exception {
		Scheme sbak=scheme;
		setScheme ((nest==0) ? Scheme.RING : Scheme.NESTED);
		Pointing[] vertex = new Pointing[3];
		vertex[0] = new Pointing(v1);
		vertex[1] = new Pointing(v2);
		vertex[2] = new Pointing(v3);
		LongRangeSet res = super.queryPolygon(vertex,(inclusive!=0));
		setScheme (sbak);
		return res;
	}

	/**
	 * finds pixels having a colatitude (measured from North pole) : theta1 <
	 * colatitude < theta2 with o <= theta1 < theta2 <= Pi if theta2 < theta1
	 * then pixels with 0 <= colatitude < theta2 or theta1 < colatitude < Pi are
	 * returned
	 * 
	 * @param nside
	 *            long the map resolution parameter
	 * @param theta1
	 *            lower edge of the colatitude
	 * @param theta2
	 *            upper edge of the colatitude
	 * @param nest
	 *            long if = 1 result is in NESTED scheme
	 * @return ArrayList of pixel numbers (long)
	 * @throws Exception
	 */
	public LongRangeSet query_strip(int nside, double theta1,
			double theta2, long nest) throws Exception {
		HealpixBase base= new HealpixBase(nside,(nest!=0)? Scheme.NESTED : Scheme.RING);
		return base.queryStrip(theta1,theta2,false);
	}

	/**
	 * returns 7 or 8 neighbours of any pixel in the nested scheme The neighbours
	 * are ordered in the following way: First pixel is the one to the south (
	 * the one west of the south direction is taken for pixels that don't have a
	 * southern neighbour). From then on the neighbors are ordered in the
	 * clockwise direction.
	 * 
	 * @param ipix long pixel number
	 * @return ArrayList
	 * @throws Exception 
	 * @throws IllegalArgumentException
	 */
	public List<Long> neighbours_nest( long ipix) throws Exception  {
		Scheme sbak=scheme;
		setScheme (Scheme.NESTED);
		long[] nb=neighbours(ipix);
		setScheme (sbak);
		ArrayList<Long> ret = new ArrayList<Long>();
		for (int i=0; i<nb.length; ++i)
			ret.add(nb[i]);
		return ret;
	}

	/**
	 * return the parent PIXEL of a given pixel at some higher NSIDE. 
	 * One must also provide the nsode of the given pixel as otherwise it
	 * can not be known.
	 * 
	 * This only makes sense for Nested Scheme.
	 * This is basically a simple bit shift in the difference
	 * of number of bits between the two NSIDEs. 
	 * 
	 * @param child  the pixel 
	 * @param childnside nside of the pixel
	 * @param requirednside nside to upgrade to
	 * 
	 * @return the new pixel number
	 * @throws Exception 
 	 */
	static public long parentAt(long child, int childnside, int requirednside) throws Exception{
	    // nside is the number of bits .. 
		if (childnside < requirednside) {
			throw new Exception ("Parent ("+requirednside+
					") should have smaller NSIDE than Child("+childnside+")");
		}
		long ppix =0;
		
		// number of bits in aid depdens on the depth of the nside

		int bitdiff = bitdiff(requirednside, childnside); 
	    ppix = child >> bitdiff;
    	return ppix;	 		
	}

	/**
	 * return difference of number of bits in pixels of two nsides.
	 * @param nside1
	 * @param nside2
	 * @return  number of bits difference between the pixel ids.
	 */
	public static int bitdiff(long nside1, long nside2){
		int pbits = 2;
		long childnside=nside2;
		long parentnside=nside1;
		if (nside1>=nside2){
			childnside=nside1;
			parentnside=nside2;
		}
		int tnside = 2;
		while (tnside < parentnside) {
			pbits+=2;
			tnside=tnside<<1 ;// next power of 2
		}
		// child is deeper 
		int cbits = pbits;
		while (tnside < childnside) {
			cbits+=2;
			tnside=tnside<<1 ;// next power of 2
		}
		return (cbits- pbits);//  
		
	}
	/**
	 * for a given pixel list all children pixels for it. 
	 * This is simply a matter of shifting the pixel number left by
	 * the difference in NSIDE bits and then listing all numbers 
	 * which fill the empty bits. 
	 * 
	 * BEWARE - not checking you are not trying to go too DEEP. 
	 * 
	 * @param nside  nside of pix
	 * @param pix  the pixel 
	 * @param requiredNside  the nside you want the children at
	 * @return children pixels
	 * @throws Exception 
	 */
	public static long[] getChildrenAt(long nside, long pix, int requiredNside) throws Exception{
	 
		if (nside >= requiredNside){
			throw new Exception("The requirend NSIDE should be greater than the pix NSIDE");
		}
		int bitdiff=bitdiff(nside,requiredNside);
		int numpix = bitdiff<<1;// square num bits is count of pix
		long[] pixlist= new long[numpix];
		long ppix=pix<<bitdiff; // shift the current pix over
		// nopw just keep adding to it ..
		for (int i=0;i < numpix; i++){
			pixlist[i]=ppix+i;
		}
		return pixlist;
	}

}
