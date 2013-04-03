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

import healpix.core.base.set.LongRangeSet;
import healpix.essentials.*;
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

   LongRangeSet toLRS (RangeSet rs)
     {
     long[] v=new long[2*rs.size()];
     for (int i=0; i<rs.size(); ++i)
       { v[2*i]=rs.ivbegin(i); v[2*i+1]=rs.ivend(i)-1; }
     return new LongRangeSet(v,v.length);
     }

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
		HealpixUtils.check(order>=0,"nside must be a power of 2");
	}

	public HealpixIndex(int nside2, Scheme sch) throws Exception{
		super (nside2, sch);
		HealpixUtils.check(order>=0,"nside must be a power of 2");
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
		return HealpixProc.ang2pixNest(order,new Pointing(theta,phi));
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
                Pointing res = HealpixProc.pix2angNest(order,ipix);
		return new double[] { res.theta, res.phi };
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
                Pointing res = HealpixProc.pix2angRing(order,ipix);
		return new double[] { res.theta, res.phi };
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
		return HealpixProc.ang2pixRing(order,new Pointing(theta,phi));
	}

	/**
	 * integration limits in cos(theta) for a given ring i_th, i_th > 0
	 *
	 * @param i_th
	 *            ith ring
	 * @return limits
	 */
	public double[] integration_limits_in_costh(int i_th) {
		double[] ret = { ring2z(i_th-1), ring2z(i_th), ring2z(i_th+1) };
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
		return HealpixProc.vec2pixNest(order,vec);
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
		return HealpixProc.vec2pixRing(order,vec);
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
		return new SpatialVector(HealpixProc.pix2vecNest(order,pix));
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
		return new SpatialVector(HealpixProc.pix2vecRing(order,pix));
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
	public SpatialVector[] corners_nest(long pix, int step) throws Exception {
		long tpix = (scheme==Scheme.NESTED) ? pix : nest2ring(pix);
		Vec3[] tvec = boundaries(tpix,step);
		SpatialVector[] res=new SpatialVector[tvec.length];
		for (int i=0; i<tvec.length; ++i)
		  res[i]=new SpatialVector(tvec[i]);
		return res;
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
		long tpix = (scheme==Scheme.RING) ? pix : ring2nest(pix);
		Vec3[] tvec = boundaries(tpix,step);
		SpatialVector[] res=new SpatialVector[tvec.length];
		for (int i=0; i<tvec.length; ++i)
		  res[i]=new SpatialVector(tvec[i]);
		return res;
	}

	/**
	 * calculates angular resolution of the pixel map in arc seconds.
	 *
	 * @param nside
	 * @return double resolution in arcsec
	 */
	static public double getPixRes(long nside) {
		double rad2arcsec=180.*60.*60./Constants.PI;
		return rad2arcsec*Math.sqrt(4*Constants.PI/(12*nside*nside));
	}

	/**
	 * calculate required nside given pixel size in arcsec
	 *
	 * @param pixsize
	 *            in arcsec
	 * @return long nside parameter
	 */
	static public int calculateNSide(double pixsize) {
		double arcsec2rad=Constants.PI/(180.*60.*60.);
		double nsd = Math.sqrt(4*Constants.PI/12.)/(arcsec2rad*pixsize);
		int order_req=Math.max(0,Math.min(order_max,1+HealpixUtils.ilog2((long)(nsd))));
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
		LongRangeSet res= toLRS((inclusive!=0) ?
                    super.queryDiscInclusive(new Pointing(vec),radius,4)
                  : super.queryDisc(new Pointing(vec),radius));
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
		return toLRS(inclusive ?
		    queryDiscInclusive(new Pointing(vec),radius,4)
                  : queryDisc(new Pointing(vec),radius));
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
		LongRangeSet res = toLRS((inclusive!=0) ?
		    super.queryPolygonInclusive(vertex,4)
		  : super.queryPolygon(vertex));
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
		LongRangeSet res = toLRS((inclusive!=0)?
		    super.queryPolygonInclusive(vertex,4)
		  : super.queryPolygon(vertex));
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
		return toLRS(base.queryStrip(theta1,theta2,false));
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
          HealpixUtils.check(childnside >= requirednside,
            "Parent ("+requirednside+") should have smaller NSIDE than Child("
            +childnside+")");
    	return child >> bitdiff(requirednside, childnside);
	}

	/**
	 * return difference of number of bits in pixels of two nsides.
	 * @param nside1
	 * @param nside2
	 * @return  number of bits difference between the pixel ids.
	 */
	public static int bitdiff(long nside1, long nside2){
		return 2*Math.abs(HealpixUtils.ilog2(nside1)-HealpixUtils.ilog2(nside2));
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
	 	HealpixUtils.check(nside<requiredNside,
		  "The requirend NSIDE should be greater than the pix NSIDE");
		int bitdiff=bitdiff(nside,requiredNside);
		int numpix = 1<<bitdiff;
		long[] pixlist= new long[numpix];
		long ppix=pix<<bitdiff; // shift the current pix over
		// now just keep adding to it ..
		for (int i=0;i < numpix; i++){
			pixlist[i]=ppix+i;
		}
		return pixlist;
	}

}
