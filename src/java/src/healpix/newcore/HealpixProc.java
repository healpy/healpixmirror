/*
 *  This file is part of Healpix Java.
 *
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
 */

package healpix.newcore;

/** Functionality related to the HEALPix pixelisation.
    This class is intended for users who prefer a procedural (instead of
    object-oriented) interface to the HEALPix functionality. It should make
    transition from packages like PixTools easier.

    @copyright 2012 Max-Planck-Society
    @author Martin Reinecke */
public abstract class HealpixProc extends HealpixBase
  {
  static final HealpixBase[] bn=new HealpixBase[HealpixBase.order_max+1],
                             br=new HealpixBase[HealpixBase.order_max+1];

  static
    {
    try {
      for (int i=0; i<=HealpixBase.order_max; ++i)
        {
        bn[i]=new HealpixBase(1L<<i, Scheme.NESTED);
        br[i]=new HealpixBase(1L<<i, Scheme.RING);
        }
      }
    catch (Exception Ex) {/*doesn't happen*/}
    }

  private HealpixProc() throws Exception {}

  private static int cilog2 (long arg) throws Exception
    {
    int res=HealpixUtils.ilog2(arg);
    if ((1L<<res)!=arg) throw new Exception ("nside is not a power of 2");
    return res;
    }

  public static long ang2pixNestO (int order, Pointing ptg) throws Exception
    { return bn[order].ang2pix(ptg); }
  public static long ang2pixNestN (long nside, Pointing ptg) throws Exception
    { return bn[cilog2(nside)].ang2pix(ptg); }
  public static long ang2pixRingO (int order, Pointing ptg) throws Exception
    { return br[order].ang2pix(ptg); }
  public static long ang2pixRingN (long nside, Pointing ptg) throws Exception
    { return br[cilog2(nside)].ang2pix(ptg); }

  public static Pointing pix2angNestO (int order, long pix) throws Exception
    { return bn[order].pix2ang(pix); }
  public static Pointing pix2angNestN (long nside, long pix) throws Exception
    { return bn[cilog2(nside)].pix2ang(pix); }
  public static Pointing pix2angRingO (int order, long pix) throws Exception
    { return br[order].pix2ang(pix); }
  public static Pointing pix2angRingN (long nside, long pix) throws Exception
    { return br[cilog2(nside)].pix2ang(pix); }

  public static long vec2pixNestO (int order, Vec3 vec) throws Exception
    { return bn[order].vec2pix(vec); }
  public static long vec2pixNestN (long nside, Vec3 vec) throws Exception
    { return bn[cilog2(nside)].vec2pix(vec); }
  public static long vec2pixRingO (int order, Vec3 vec) throws Exception
    { return br[order].vec2pix(vec); }
  public static long vec2pixRingN (long nside, Vec3 vec) throws Exception
    { return br[cilog2(nside)].vec2pix(vec); }

  public static Vec3 pix2vecNestO (int order, long pix) throws Exception
    { return bn[order].pix2vec(pix); }
  public static Vec3 pix2vecNestN (long nside, long pix) throws Exception
    { return bn[cilog2(nside)].pix2vec(pix); }
  public static Vec3 pix2vecRingO (int order, long pix) throws Exception
    { return br[order].pix2vec(pix); }
  public static Vec3 pix2vecRingN (long nside, long pix) throws Exception
    { return br[cilog2(nside)].pix2vec(pix); }

  public static long ring2nestO (int order, long pix) throws Exception
    { return bn[order].ring2nest(pix); }
  public static long ring2nestN (long nside, long pix) throws Exception
    { return bn[cilog2(nside)].ring2nest(pix); }

  public static long nest2ringO (int order, long pix) throws Exception
    { return bn[order].nest2ring(pix); }
  public static long nest2ringN (long nside, long pix) throws Exception
    { return bn[cilog2(nside)].nest2ring(pix); }

  public static long[] neighboursNestO (int order, long pix) throws Exception
    { return bn[order].neighbours(pix); }
  public static long[] neighboursNestN (long nside, long pix) throws Exception
    { return bn[cilog2(nside)].neighbours(pix); }

  public static long[] neighboursRingO (int order, long pix) throws Exception
    { return br[order].neighbours(pix); }
  public static long[] neighboursRingN (long nside, long pix) throws Exception
    { return br[cilog2(nside)].neighbours(pix); }

  public static Vec3[] boundariesNestO(int order, long pix, int step)
    throws Exception
    { return bn[order].boundaries(pix,step); }
  public static Vec3[] boundariesNestN(long nside, long pix, int step)
    throws Exception
    { return bn[cilog2(nside)].boundaries(pix,step); }

  public static Vec3[] boundariesRingO(int order, long pix, int step)
    throws Exception
    { return br[order].boundaries(pix,step); }
  public static Vec3[] boundariesRingN(long nside, long pix, int step)
    throws Exception
    { return br[cilog2(nside)].boundaries(pix,step); }

  public static RangeSet queryDiscNestO(int order, Pointing ptg, double radius,
    boolean inclusive) throws Exception
    { return bn[order].queryDisc(ptg,radius,inclusive); }
  public static RangeSet queryDiscNestN(long nside, Pointing ptg, double radius,
    boolean inclusive) throws Exception
    { return bn[cilog2(nside)].queryDisc(ptg,radius,inclusive); }

  public static RangeSet queryDiscRingO(int order, Pointing ptg, double radius,
    boolean inclusive) throws Exception
    { return br[order].queryDisc(ptg,radius,inclusive); }
  public static RangeSet queryDiscRingN(long nside, Pointing ptg, double radius,
    boolean inclusive) throws Exception
    { return br[cilog2(nside)].queryDisc(ptg,radius,inclusive); }

  public static RangeSet queryPolygonNestO(int order, Pointing[] vertex,
    boolean inclusive) throws Exception
    { return bn[order].queryPolygon(vertex,inclusive); }
  public static RangeSet queryPolygonNestN(long nside, Pointing[] vertex,
    boolean inclusive) throws Exception
    { return bn[cilog2(nside)].queryPolygon(vertex,inclusive); }

  public static RangeSet queryPolygonRingO(int order, Pointing[] vertex,
    boolean inclusive) throws Exception
    { return br[order].queryPolygon(vertex,inclusive); }
  public static RangeSet queryPolygonRingN(long nside, Pointing[] vertex,
    boolean inclusive) throws Exception
    { return br[cilog2(nside)].queryPolygon(vertex,inclusive); }
  };
