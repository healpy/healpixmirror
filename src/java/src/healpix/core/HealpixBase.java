/*
 * Experimental HEALPix Java code derived from the Gaia-developed Java sources
 * and the Healpix C++ library.
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

package healpix.core;

import java.util.Arrays;

import healpix.core.base.set.*;
import healpix.core.dm.AbstractHealpixMap.Scheme;

/** Functionality related to the HEALPix pixelisation.
    This class is conceptually very similar the the Healpix_Base class
    of Healpix_cxx. It supports NESTED and RING scheme for nside parameters
    which are powers of 2, and RING scheme for all other nside parameters.
    @author Martin Reinecke */
public class HealpixBase extends HealpixTables
  {
  protected static class Xyf
    {
    public int ix, iy, face;
    public Xyf () {}
    public Xyf (int x, int y, int f)
      { ix=x; iy=y; face=f; }
    }

  protected static class RingInfoSmall
    {
    long startpix, ringpix;
    boolean shifted;
    }

  /** Maximum order parameter */
  public static final int order_max=29;
  /** Maximum Nside parameter; equivalent to 2^{@code order_max}. */
  public static final long ns_max=1L<<order_max;

  /** The order of the map; -1 for nonhierarchical map. */
  protected int order;

  /** The Nside parameter. */
  protected long nside;

  protected long nl2, nl3, nl4, npface, npix, ncap;
  protected double fact1, fact2;
  protected Scheme scheme;

  private static long spread_bits (int v)
    {
    return (long)(utab[ v      &0xff])      | ((long)(utab[(v>>> 8)&0xff])<<16)
        | ((long)(utab[(v>>>16)&0xff])<<32) | ((long)(utab[(v>>>24)&0xff])<<48);
    }
  private static int compress_bits (long v)
    {
    long raw = v&0x5555555555555555L;
    raw |= raw>>>15;
    int raw1 = (int)(raw&0xffffL), raw2 = (int)((raw>>>32)&0xffffL);
    return ctab[raw1&0xff]      | (ctab[raw1>>>8]<< 4)
        | (ctab[raw2&0xff]<<16) | (ctab[raw2>>>8]<<20);
    }

  private Xyf nest2xyf(long ipix)
    {
    long pix=ipix&(npface-1);
    return new Xyf (compress_bits(pix), compress_bits(pix>>>1),
                    (int)(ipix>>>(2*order)));
    }

  private long xyf2nest(int ix, int iy, int face_num)
    {
    return ((long)(face_num)<<(2*order)) +
      spread_bits(ix) + (spread_bits(iy)<<1);
    }

  private long xyf2ring(int ix, int iy, int face_num)
    {
    long jr = ((long)jrll[face_num]*nside) - (long)ix - (long)iy  - 1L;

    RingInfoSmall ris = get_ring_info_small(jr);
    long nr=ris.ringpix>>>2, kshift=ris.shifted ? 0:1;

    long jp = (jpll[face_num]*nr + (long)ix - (long)iy + 1L + kshift) / 2L;
    if (jp>nl4)
      jp-=nl4;
    else
      if (jp<1) jp+=nl4;

    return ris.startpix + jp - 1L;
    }

  private Xyf ring2xyf(long pix)
    {
    Xyf ret = new Xyf();
    long iring, iphi, kshift, nr;

    if (pix<ncap) // North Polar cap
      {
      iring = (1+HealpixUtils.isqrt(1L+2L*pix))>>>1; //counted from North pole
      iphi = (pix+1) - 2*iring*(iring-1);
      kshift = 0;
      nr = iring;
      ret.face=(int)((iphi-1)/nr);
      }
    else if (pix<(npix-ncap)) // Equatorial region
      {
      long ip = pix - ncap;
      long tmp = (order>=0) ? ip>>>(order+2) : ip/nl4;
      iring = tmp+nside;
      iphi = ip-tmp*nl4 + 1;
      kshift = (iring+nside)&1;
      nr = nside;
      long ire = iring-nside+1,
           irm = nl2+2-ire;
      long ifm = iphi - (ire>>>1) + nside -1,
           ifp = iphi - (irm>>>1) + nside -1;
      if (order>=0)
        { ifm >>>= order; ifp >>>= order; }
      else
        { ifm /= nside; ifp /= nside; }
      ret.face = (int)((ifp==ifm) ? (ifp|4) : ((ifp<ifm) ? ifp : (ifm+8)));
      }
    else // South Polar cap
      {
      long ip = npix - pix;
      iring = (1+HealpixUtils.isqrt(2L*ip-1L))>>>1; //counted from South pole
      iphi  = 4L*iring + 1 - (ip - 2L*iring*(iring-1L));
      kshift = 0;
      nr = iring;
      iring = 2L*nl2-iring;
      ret.face = 8 + (int)((iphi-1)/nr);
      }

    long irt = iring - ((long)jrll[ret.face]*nside) + 1L;
    long ipt = 2L*iphi- (long)jpll[ret.face]*nr - kshift -1L;
    if (ipt>=nl2) ipt-=8L*nside;

    ret.ix = (int)(( ipt-irt)>>>1);
    ret.iy = (int)((-ipt-irt)>>>1);

    return ret;
    }

  /** Calculates the map order from its Nside parameter.
      @param nside the Nside parameter
      @return the map order corresponding to {@code nside}; -1 if
               {@code nside} is not a power of 2. */
  public static int nside2order(long nside)
    {
    assert (nside>0);
    return ((nside&(nside-1))!=0) ? -1 : HealpixUtils.ilog2(nside);
    }

  /** Calculates the Nside parameter from the number of pixels.
      @param npix the number of pixels
      @return the computed Nside parameter */
  public static long npix2Nside(long npix) throws Exception
    {
    long nside = HealpixUtils.isqrt(npix/12);
    HealpixUtils.check(12*nside*nside==npix,"npix is not 12*nside*nside");
    HealpixUtils.check(nside<=ns_max,"nside is too large");
    return nside;
    }
  /** Calculates the number of pixels from the Nside parameter.
      @param nside the Nside parameter
      @return the computed number of pixels */
  public static long nside2Npix(long nside) throws Exception
    {
    HealpixUtils.check(nside<=ns_max,"nside is too large");
    return 12*nside*nside;
    }

  /** Adjusts the object to nside_in.
      @param nside_in the new Nside parameter */
  public void setNside (long nside_in) throws Exception
    {
    if (nside==nside_in) return;
    nside=nside_in;
    HealpixUtils.check(nside<=ns_max && nside>0,
      "Nside must be between 1 and " + ns_max);

    order = nside2order(nside);
    if (scheme==Scheme.NESTED && order<0)
      throw new Exception("Nside must be a power of 2 for NESTED scheme");
    nl2 = 2*nside;
    nl3 = 3*nside;
    nl4 = 4*nside;
    npface = nside*nside;
    ncap = 2*nside*(nside-1); // pixels in each polar cap
    npix = 12*npface;
    fact2 = 4.0/npix;
    fact1 = (nside<<1)*fact2;
    }

  /** Adjusts the object to nside_in and scheme_in.
      @param nside_in the new Nside parameter
      @param scheme_in the new ordering scheme */
  public void setNsideAndScheme (long nside_in, Scheme scheme_in)
    throws Exception
    {
    if ((scheme==scheme_in) && (nside==nside_in)) return;
    HealpixUtils.check (((nside_in&(nside_in-1))==0)||(scheme_in==Scheme.RING),
      "Nside must be a power of 2 for NESTED scheme");
    scheme=scheme_in;
    setNside(nside_in);
    }

  /** Initializes the object to Nside=1024 and scheme=NESTED. */
  public HealpixBase()
    {
    try
      {
      nside=-1;
      setNsideAndScheme(1024,Scheme.NESTED);
      }
    catch (Exception Ex) { /* cannot happen */ }
    }


  /** Initializes the object to a user-supplied Nside and ordering scheme.
      @param nside_in the Nside parameter
      @param scheme_in the ordering scheme */
  public HealpixBase(long nside_in, Scheme scheme_in) throws Exception
    { nside=nside_in-1; setNsideAndScheme(nside_in, scheme_in); }

  /** Returns the current ordering scheme.
      @return the current ordering scheme */
  public Scheme getScheme()
    { return scheme; }

  /** Returns the current Nside parameter.
      @return the current Nside parameter */
  public int getNside()
    { return (int)nside; }

  /** Returns the total number of pixels in the pixelisation.
      @return the current total number of pixels */
  public long getNpix()
    { return npix; }

  /** Adjusts the object to scheme.
      @param scheme_in the new ordering scheme */
  public void setScheme(Scheme scheme_in) throws Exception
    {
    if (scheme_in==Scheme.NESTED && order<0)
      throw new Exception("Nside must be a power of 2 for NESTED scheme");
    scheme = scheme_in;
    }

  /** Returns the current order parameter.
      @return the current map order parameter. */
  public int getOrder()
    { return order; }

  /** Returns the pixel which contains the supplied Pointing.
      @param ptg the requested location on the sphere.
      @return the pixel number containing the location. */
  public long ang2pix(Pointing ptg) throws Exception
    { return zphi2pix(new Zphi(ptg)); }

  /** Returns the Pointing corresponding to the center of the supplied pixel.
      @param pix the requested pixel number.
      @return the pixel's center coordinates. */
  public Pointing pix2ang(long pix) throws Exception
    { return new Pointing(pix2zphi(pix)); }

  /** Returns the pixel which contains the supplied Vec3.
      @param vec the requested location on the sphere (need not be normalized).
      @return the pixel number containing the location. */
  public long vec2pix(Vec3 vec) throws Exception
    { return zphi2pix(new Zphi(vec)); }

  /** Returns the normalized 3-vector corresponding to the center of the
      supplied pixel.
      @param pix the requested pixel number.
      @return the pixel's center coordinates. */
  public Vec3 pix2vec(long pix) throws Exception
    { return new Vec3(pix2zphi(pix)); }

  /** Returns nested pixel number for the supplied ring pixel number.
      @param ipring the requested pixel number in RING scheme.
      @return the corresponding pixel number in NESTED scheme. */
  public long ring2nest(long ipring) throws Exception
    {
    Xyf xyf = ring2xyf(ipring);
    return xyf2nest (xyf.ix,xyf.iy, xyf.face);
    }

  /** Returns ring pixel number for the supplied nested pixel number.
      @param ipnest the requested pixel number in NESTED scheme.
      @return the corresponding pixel number in RING scheme. */
  public long nest2ring(long ipnest) throws Exception
    {
    Xyf xyf = nest2xyf(ipnest);
    return xyf2ring (xyf.ix,xyf.iy, xyf.face);
    }

  /** Returns the pixel which contains the supplied Zphi.
      @param zphi the requested location on the sphere.
      @return the pixel number containing the location. */
  public long zphi2pix (Zphi zphi)
    {
    double z=zphi.z, phi=zphi.phi;

    double za = Math.abs(z);
    double tt = HealpixUtils.fmodulo((phi*Constants.inv_halfpi),4.0);// in [0,4)

    if (scheme==Scheme.RING)
      {
      if (za<=Constants.twothird) // Equatorial region
        {
        double temp1 = nside*(0.5+tt);
        double temp2 = nside*z*0.75;
        long jp = (long)(temp1-temp2); // index of  ascending edge line
        long jm = (long)(temp1+temp2); // index of descending edge line

        // ring number counted from z=2/3
        long ir = nside + 1 + jp - jm; // in {1,2n+1}
        long kshift = 1-(ir&1); // kshift=1 if ir even, 0 otherwise

        long t1 = jp+jm-nside+kshift+1+nl4+nl4;
        long ip = (order>0) ?
          (t1>>>1)&(nl4-1) : ((t1>>>1)%nl4); // in {0,4n-1}

        return ncap + (ir-1)*nl4 + ip;
        }
      else  // North & South polar caps
        {
        double tp = tt-(long)(tt);
        double tmp = nside*Math.sqrt(3*(1-za));

        long jp = (long)(tp*tmp); // increasing edge line index
        long jm = (long)((1.0-tp)*tmp); // decreasing edge line index

        long ir = jp+jm+1; // ring number counted from the closest pole
        long ip = (long)(tt*ir); // in {0,4*ir-1}
        assert((ip>=0)&&(ip<4*ir));

        return (z>0)  ?  2*ir*(ir-1) + ip  :  npix - 2*ir*(ir+1) + ip;
        }
      }
    else // scheme_ == NEST
      {
      if (za<=Constants.twothird) // Equatorial region
        {
        double temp1 = nside*(0.5+tt);
        double temp2 = nside*(z*0.75);
        long jp = (long)(temp1-temp2); // index of  ascending edge line
        long jm = (long)(temp1+temp2); // index of descending edge line
        long ifp = jp >>> order;  // in {0,4}
        long ifm = jm >>> order;
        long face_num = (ifp==ifm) ? (ifp|4) : ((ifp<ifm) ? ifp : (ifm+8));

        long ix = jm & (nside-1),
             iy = nside - (jp & (nside-1)) - 1;
        return xyf2nest((int)ix,(int)iy,(int)face_num);
        }
      else // polar region, za > 2/3
        {
        int ntt = Math.min(3,(int)tt);
        double tp = tt-ntt;
        double tmp = nside*Math.sqrt(3*(1-za));

        long jp = (long)(tp*tmp); // increasing edge line index
        long jm = (long)((1.0-tp)*tmp); // decreasing edge line index
        if (jp>=nside) jp = nside-1; // for points too close to the boundary
        if (jm>=nside) jm = nside-1;
        return (z>=0) ?
          xyf2nest((int)(nside-jm -1),(int)(nside-jp-1),ntt) :
          xyf2nest((int)jp,(int)jm,ntt+8);
        }
      }
    }

  /** Returns the Zphi corresponding to the center of the supplied pixel.
      @param pix the requested pixel number.
      @return the pixel's center coordinates. */
  public Zphi pix2zphi (long pix)
    {
    double z,phi;
    if (scheme==Scheme.RING)
      {
      if (pix<ncap) // North Polar cap
        {
        long iring = (1+(HealpixUtils.isqrt(1+2*pix)))>>>1; //counted from North pole
        long iphi  = (pix+1) - 2*iring*(iring-1);

        z = 1.0 - (iring*iring)*fact2;
        phi = (iphi-0.5) * Constants.halfpi/iring;
        }
      else if (pix<(npix-ncap)) // Equatorial region
        {
        long ip  = pix - ncap;
        long tmp = (order>=0) ? ip>>>(order+2) : ip/nl4;
        long iring = tmp + nside,
          iphi = ip-nl4*tmp+1;;
        // 1 if iring+nside is odd, 1/2 otherwise
        double fodd = ((iring+nside)&1)!=0 ? 1 : 0.5;

        z = (nl2-iring)*fact1;
        phi = (iphi-fodd) * Math.PI*0.75*fact1;
        }
      else // South Polar cap
        {
        long ip = npix - pix;
        long iring = (1+HealpixUtils.isqrt(2*ip-1))>>>1; //counted from South pole
        long iphi  = 4*iring + 1 - (ip - 2*iring*(iring-1));

        z = -1.0 + iring*iring*fact2;
        phi = (iphi-0.5) * Constants.halfpi/iring;
        }
      }
    else
      {
      Xyf xyf= nest2xyf(pix);

      long jr = ((long)(jrll[xyf.face])<<order) -xyf.ix - xyf.iy - 1;

      long nr;
      if (jr<nside)
        {
        nr = jr;
        z = 1 - nr*nr*fact2;
        }
      else if (jr>nl3)
        {
        nr = nl4-jr;
        z = nr*nr*fact2 - 1;
        }
      else
        {
        nr = nside;
        z = (nl2-jr)*fact1;
        }

      long tmp=(long)(jpll[xyf.face])*nr+xyf.ix-xyf.iy;
      assert(tmp<8*nr);//,"must not happen");
      if (tmp<0) tmp+=8*nr;
      phi = (nr==nside) ? 0.75*Constants.halfpi*tmp*fact1 :
                          (0.5*Constants.halfpi*tmp)/nr;
      }
    return new Zphi(z,phi);
    }

  /** Returns the neighboring pixels of ipix.
      This method works in both RING and NEST schemes, but is
      considerably faster in the NEST scheme.
      @param ipix the requested pixel number.
      @return array with indices of the neighboring pixels.
        The returned array contains (in this order)
        the pixel numbers of the SW, W, NW, N, NE, E, SE and S neighbor
        of ipix. If a neighbor does not exist (this can only be the case
        for the W, N, E and S neighbors), its entry is set to -1. */
  public long[] neighbours(long ipix) throws Exception
    {
    long[] result = new long[8];
    Xyf xyf = (scheme==Scheme.NESTED) ? nest2xyf(ipix) : ring2xyf(ipix);
    int ix = xyf.ix, iy=xyf.iy, face_num=xyf.face;

    long nsm1 = nside-1;
    if ((ix>0)&&(ix<nsm1)&&(iy>0)&&(iy<nsm1))
      {
      if (scheme==Scheme.RING)
        for (int m=0; m<8; ++m)
          result[m]=(xyf2ring(ix+xoffset[m],iy+yoffset[m],face_num));
      else
        {
        long fpix = (long)(face_num)<<(2*order),
             px0=spread_bits(ix  ), py0=spread_bits(iy  )<<1,
             pxp=spread_bits(ix+1), pyp=spread_bits(iy+1)<<1,
             pxm=spread_bits(ix-1), pym=spread_bits(iy-1)<<1;

        result[0]=fpix+pxm+py0; result[1]=fpix+pxm+pyp;
        result[2]=fpix+px0+pyp; result[3]=fpix+pxp+pyp;
        result[4]=fpix+pxp+py0; result[5]=fpix+pxp+pym;
        result[6]=fpix+px0+pym; result[7]=fpix+pxm+pym;
        }
      }
    else
      {
      for (int i=0; i<8; ++i)
        {
        int x=ix+xoffset[i];
        int y=iy+yoffset[i];
        int nbnum=4;
        if (x<0)
          { x+=nside; nbnum-=1; }
        else if (x>=nside)
          { x-=nside; nbnum+=1; }
        if (y<0)
          { y+=nside; nbnum-=3; }
        else if (y>=nside)
          { y-=nside; nbnum+=3; }

        int f = facearray[nbnum][face_num];

        if (f>=0)
          {
          int bits = swaparray[nbnum][face_num>>>2];
          if ((bits&1)>0) x=(int)(nside-x-1);
          if ((bits&2)>0) y=(int)(nside-y-1);
          if ((bits&4)>0) { int tint=x; x=y; y=tint; }
          result[i] = (scheme==Scheme.NESTED) ?
            xyf2nest(x,y,f) : xyf2ring(x,y,f);
          }
        else
          result[i]=-1;
        }
      }
    return result;
    }

  /** Returns the maximum angular distance between a pixel center and its
      corners.
      @return maximum angular distance between a pixel center and its
        corners. */
  public double maxPixrad()
    {
    Vec3 va = new Vec3(new Zphi(2./3., Math.PI/nl4));
    double t1 = 1.-1./nside;
    t1*=t1;
    Vec3 vb = new Vec3(new Zphi(1-t1/3, 0));
    return va.angle(vb);
    }

  private RingInfoSmall get_ring_info_small (long ring)
    {
    RingInfoSmall ret = new RingInfoSmall();
    if (ring<nside)
      {
      ret.shifted = true;
      ret.ringpix = 4*ring;
      ret.startpix = 2*ring*(ring-1);
      }
    else if (ring<nl3)
      {
      ret.shifted = ((ring-nside) & 1) == 0;
      ret.ringpix = nl4;
      ret.startpix = ncap + (ring-nside)*nl4;
      }
    else
      {
      ret.shifted = true;
      long nr=nl4-ring;
      ret.ringpix = 4*nr;
      ret.startpix = npix-2*nr*(nr+1);
      }
    return ret;
    }

  protected void inRing(long iz, double phi0, double dphi,
    LongRangeSetBuilder res)
    {
    RingInfoSmall ris = get_ring_info_small(iz);
    double shift = ris.shifted ? 0.5 : 0.;
    long nr = ris.ringpix, ipix1=ris.startpix;
    long ipix2 = ipix1 + nr - 1; // highest pixel number in the ring

    if (dphi > (Constants.pi-1e-12))
      res.appendRange(ipix1,ipix2);
    else
      {
      long ip_lo = (long) Math.floor(nr/Constants.twopi*(phi0-dphi) - shift)+1;
      long ip_hi = (long) Math.floor(nr/Constants.twopi*(phi0+dphi) - shift);

      if (ip_hi < ip_lo ) return;

      if (ip_hi>=nr)
        { ip_lo-=nr; ip_hi-=nr; }

      if (ip_lo<0)
        {
        res.appendRange(ipix1,ipix1+ip_hi);
        res.appendRange(ipix1+ip_lo+nr,ipix2);
        }
      else
        res.appendRange(ipix1+ip_lo,ipix1+ip_hi);
      }
    }

  private long ringAbove (double z)
    {
    double az=Math.abs(z);
    if (az> Constants.twothird) // polar caps
      {
      long iring = (long)(nside*Math.sqrt(3*(1-az)));
      return (z>0) ? iring : nl4-iring-1;
      }
    else // equatorial region
      return (long)(nside*(2.0-1.5*z));
    }

  /** Returns the z-coordinate (equivalent to the cosine of the colatitude)
      for a requested ring.
      This method also accepts the not-really-existing ring indices 0 and
      4*Nside, which correspond to North and South Poles, respectively.
      @param ring ring index: the northernmost ring in the map has index 1;
        ring indices are increasing towards the South pole.
      @return z-coordinate of the ring. */
  public double ring2z (long ring)
    {
    if (ring<nside)
      return 1 - ring*ring*fact2;
    if (ring <=nl3)
      return (nl2-ring)*fact1;
    ring=nl4 - ring;
    return ring*ring*fact2 - 1;
    }

  private static class pstack
    {
    private long[] p;
    private int[] o;
    private int s, m;

    public pstack (int sz)
      { p=new long[sz]; o=new int[sz]; s=m=0; }
    public void push (long p_, int o_)
      { p[s]=p_; o[s]=o_; ++s; }
    public void resize (int s_)
      { s=s_; }
    public void pop ()
      { --s; }
    public void popToMark ()
      { s=m; }
    public int size ()
      { return s; }
    public void mark ()
      { m=s; }
    public int otop ()
      { return o[s-1]; }
    public long ptop ()
      { return p[s-1]; }
    }

  /** Returns a range set of pixels whose centers lie within a given disk on the
      sphere (if {@code inclusive==false}), or which overlap with this disk
      (if {@code inclusive==true}).<p>
      This method is more efficient in the RING scheme, but the
      algorithm used for {@code inclusive==true} returns fewer false positives
      in the NESTED scheme.
      @param ptg the angular coordinates of the disk center
      @param radius the radius (in radians) of the disk
      @param inclusive if {@code false}, return the exact set of pixels whose
        pixels centers lie within the disk; if {@code true}, return all pixels
        that overlap with the disk, and maybe a few more.
      @return an object containing the indices of all pixels within the disk */
  public LongRangeSet queryDisc(Pointing ptg, double radius, boolean inclusive)
    throws Exception
    {
    LongRangeSetBuilder pixset = new LongRangeSetBuilder();
    if (scheme==Scheme.RING)
      {
      if (inclusive) radius+=maxPixrad();
      if (radius>=Constants.pi)
        { pixset.appendRange(0,npix-1); return pixset.build(); }

      double cosang = Math.cos(radius);

      double z0 = Math.cos(ptg.theta);
      double xa = 1./Math.sqrt((1-z0)*(1+z0));

      double rlat1  = ptg.theta - radius;
      double zmax = Math.cos(rlat1);
      long irmin = ringAbove (zmax)+1;

      if ((rlat1<=0) && (irmin>1)) // north pole in the disk
        {
        //get_ring_info_small(irmin-1,sp,rp,dummy);
        RingInfoSmall info =get_ring_info_small(irmin-1);
        pixset.appendRange(0,info.startpix+info.ringpix-1);
        }

      double rlat2  = ptg.theta + radius;
      double zmin = Math.cos(rlat2);
      long irmax = ringAbove (zmin);

      for (long iz=irmin; iz<=irmax; ++iz) // rings partially in the disk
        {
        double z=ring2z(iz);

        double x = (cosang-z*z0)*xa;
        double ysq = 1-z*z-x*x;
        assert(ysq>=0);
        double dphi=Math.atan2(Math.sqrt(ysq),x);
        inRing (iz, ptg.phi, dphi, pixset);
        }

      if ((rlat2>=Constants.pi) && (irmax+1<nl4)) // south pole in the disk
        {
        // get_ring_info_small(irmax+1,sp,rp,dummy);
        RingInfoSmall info =get_ring_info_small(irmax+1);
        pixset.appendRange(info.startpix,npix-1);
        }
      }
    else // scheme_==NEST
      {
      if (radius>=Constants.pi) // disk covers the whole sphere
        { pixset.appendRange(0,npix-1); return pixset.build(); }

      int oplus=inclusive ? 2 : 0;
      int omax=Math.min(order_max,order+oplus); // the order up to which we test

      Vec3 vptg = new Vec3(ptg);

      //arr<T_Healpix_Base<I> > base(omax+1);
      HealpixBase[] base = new HealpixBase[omax+1];

      //arr<double> crpdr(omax+1), crmdr(omax+1);
      double[] crpdr = new double[omax+1];
      double[] crmdr = new double[omax+1];

      double cosrad=Math.cos(radius);
      for (int o=0; o<=omax; o++) // prepare data at the required orders
        {
        base[o]=new HealpixBase(1L<<o,Scheme.NESTED);
        double dr=base[o].maxPixrad(); // safety distance
        crpdr[o] = (radius+dr>Constants.pi) ? -1. : Math.cos(radius+dr);
        crmdr[o] = (radius-dr<0.) ?  1. : Math.cos(radius-dr);
        }

      //vector<pair<I,int> > stk; // stack for pixel numbers and their orders
      pstack stk=new pstack(12+3*omax);
      for (int i=0; i<12; i++) { // insert the 12 base pixels in reverse order
        stk.push(11-i,0);
      }

      while (stk.size()>0) {// as long as there are pixels on the stack
        // pop current pixel number and order from the stack
        long pix=stk.ptop();
        int o=stk.otop();
        stk.pop();

        Zphi pos=base[o].pix2zphi(pix);
        // cosine of angular distance between pixel center and disk center
        // double cangdist=cosdist_zphi(vptg.z,ptg.phi,z,phi);
        double cangdist=HealpixUtils.cosdist_zphi(vptg.z,ptg.phi,pos.z,pos.phi);

        if (cangdist>crpdr[o])
          {
          int zone = (cangdist<cosrad) ? 1 : ((cangdist<=crmdr[o]) ? 2 : 3);

          check_pixel (o, omax, zone, pixset, pix, stk, inclusive);
          }
        }
      }
    return pixset.build();
    }

  private LongRangeSet queryMultiDisc (Vec3[] norm, double[] rad,
    boolean inclusive) throws Exception
    {
    int nv=norm.length;
    HealpixUtils.check(nv==rad.length,"inconsistent input arrays");
    LongRangeSetBuilder res = new LongRangeSetBuilder();

    if (scheme==Scheme.RING)
      {
      double rplus = inclusive ? maxPixrad() : 0;
      long irmin=1, irmax=nl4-1;
      double thmin=0, thmax=Constants.pi;
      int nd=0;
      double[] z0=new double[nv];
      double[] xa=new double[nv];
      double[] cosrad=new double[nv];
      Pointing[] ptg=new Pointing[nv];
      for (int i=0; i<nv; ++i)
        {
        double r=rad[i]+rplus;
        if (r<Constants.pi)
          {
          Pointing pnt= new Pointing(norm[i]);
          cosrad[i]=Math.cos(r);
          double cth=Math.cos(pnt.theta);
          z0[nd]=cth;
          xa[nd]=1./Math.sqrt((1-cth)*(1+cth));
          ptg[nd]=pnt;
          double tmp = Math.min(pnt.theta+r,Constants.pi);
          if (tmp < thmax)
            { thmax=tmp; irmax=ringAbove(Math.cos(thmax)); }
          tmp = Math.max(0.,pnt.theta-r);
          if (tmp > thmin)
            { thmin=tmp; irmin=ringAbove(Math.cos(thmin))+1; }
          ++nd;
          }
        }

      for (long iz=irmin; iz<=irmax; ++iz)
        {
        double z=ring2z(iz);
        RingInfoSmall ris=get_ring_info_small(iz);
        long ipix1=ris.startpix, nr=ris.ringpix;
        long ipix2 = ipix1 + nr - 1; // highest pixel number in the ring
        double shift = ris.shifted ? 0.5 : 0.;
        LongRangeSetBuilder tr = new LongRangeSetBuilder();
        tr.appendRange(ipix1,ipix2);
        LongRangeSet rstmp = tr.build();

        for (int j=0; (j<nd)&&(rstmp.size()>0); ++j)
          {
          double x = (cosrad[j]-z*z0[j])*xa[j];
          double ysq = 1.-z*z-x*x;
          if (ysq>=0.)
            {
            double dphi=Math.atan2(Math.sqrt(ysq),x);

            if (dphi < (Constants.pi-1e-12))
              {
              long ip_lo = (long)Math.floor
                (nr*Constants.inv_twopi*(ptg[j].phi-dphi)-shift)+1;
              long ip_hi = (long)Math.floor
                (nr*Constants.inv_twopi*(ptg[j].phi+dphi)-shift);
              if (ip_hi>=nr)
                { ip_lo-=nr; ip_hi-=nr;}

//FIXME: should be replaced by clear() method once it's available.
              tr = new LongRangeSetBuilder();
              if (ip_lo<0)
                {
                if (ip_hi+1<=ip_lo+nr-1)
                  tr.appendRange(ipix1+ip_hi+1,ipix1+ip_lo+nr-1);
                }
              else
                {
                if (ip_lo>0)
                  tr.appendRange(ipix1,ipix1+ip_lo-1);
                if (ipix1+ip_hi+1<=ipix2)
                  tr.appendRange(ipix1+ip_hi+1,ipix2);
                }
              if (tr.size()>0)
                rstmp=rstmp.substract(tr.build());
              }
            }
          }
        res.appendRangeSet(rstmp);
        }
      }
    else // scheme_ == NEST
      {
      int oplus=inclusive ? 2 : 0;
      int omax=Math.min(order_max,order+oplus); // the order up to which we test

      // TODO: ignore all disks with radius>=pi

      HealpixBase[] base= new HealpixBase[omax+1];
      double[][][] crlimit=new double[omax+1][nv][3];
      for (int o=0; o<=omax; ++o) // prepare data at the required orders
        {
        base[o]=new HealpixBase(1L<<o,Scheme.NESTED);
        double dr=base[o].maxPixrad(); // safety distance
        for (int i=0; i<nv; ++i)
          {
          crlimit[o][i][0] = (rad[i]+dr>Constants.pi) ? -1: Math.cos(rad[i]+dr);
          crlimit[o][i][1] = (o==0) ? Math.cos(rad[i]) : crlimit[0][i][1];
          crlimit[o][i][2] = (rad[i]-dr<0.) ?  1. : Math.cos(rad[i]-dr);
          }
        }

      pstack stk=new pstack(12+3*omax);
      for (int i=0; i<12; i++) { // insert the 12 base pixels in reverse order
        stk.push(11-i,0);
      }

      while (stk.size()>0) {// as long as there are pixels on the stack
        // pop current pixel number and order from the stack
        long pix=stk.ptop();
        int o=stk.otop();
        stk.pop();

        Vec3 pv = base[o].pix2vec(pix);

        int zone=3;
        for (int i=0; (i<nv)&&(zone>0); ++i)
          {
          double crad=pv.dot(norm[i]);
          for (int iz=0; iz<zone; ++iz)
            if (crad<crlimit[o][i][iz])
              zone=iz;
          }

        if (zone>0) check_pixel (o, omax, zone, res, pix, stk, inclusive);
        }
      }
    return res.build();
    }

  /** Returns a range set of pixels whose centers lie within the convex
      polygon defined by the {@code vertex} array (if
      {@code inclusive==false}), or which overlap with this polygon
      (if {@code inclusive==true}).
      @param vertex an array containing the vertices of the requested convex
        polygon.
      @param inclusive if {@code false}, return the exact set of pixels whose
        pixel centers lie within the polygon; if {@code true}, return all
        pixels that overlap with the polygon, and maybe a few more.
      @return an object containing the indices of all pixels within the
        polygon */
  public LongRangeSet queryPolygon (Pointing[] vertex, boolean inclusive)
    throws Exception
    {
    int nv=vertex.length;
    int ncirc = inclusive ? nv+1 : nv;
    HealpixUtils.check(nv>=3,"not enough vertices in polygon");
    Vec3[] vv = new Vec3[nv];
    for (int i=0; i<nv; ++i)
      vv[i] = new Vec3(vertex[i]);
    Vec3[] normal = new Vec3[ncirc];
    int flip=0;
    for (int i=0; i<nv; ++i)
      {
      normal[i]=vv[i].cross(vv[(i+1)%nv]);
      double hnd=normal[i].dot(vv[(i+2)%nv]);
      HealpixUtils.check(Math.abs(hnd)>1e-10,"degenerate corner");
      if (i==0)
        flip = (hnd<0.) ? -1 : 1;
      else
        HealpixUtils.check(flip*hnd>0,"polygon is not convex");
      normal[i].scale(flip/normal[i].length());
      }
    double[] rad = new double[ncirc];
    Arrays.fill(rad,Constants.halfpi);
    if (inclusive)
      {
      CircleFinder cf = new CircleFinder(vv);
      normal[nv]=cf.center;
      rad[nv]=Math.acos(cf.cosrad);
      }
    return queryMultiDisc(normal,rad,inclusive);
    }

  private void check_pixel (int o, int omax, int zone,
    LongRangeSetBuilder pixset, long pix, pstack stk, boolean inclusive)
    {
    if (zone==0) return;

    if (o<order)
      {
      if (zone>=3) // output all subpixels
        {
        int sdist=2*(order-o); // the "bit-shift distance" between map orders
        pixset.appendRange(pix<<sdist,((pix+1)<<sdist)-1);
        }
      else // (zone>=1)
        for (int i=0; i<4; ++i)
          stk.push(4*pix+3-i,o+1); // add children
      }
    else if (o>order) // this implies that inclusive==true
      {
      if (zone>=2) // pixel center in shape
        {
        pixset.append(pix>>>(2*(o-order))); // output the parent pixel at order
        stk.popToMark(); // unwind the stack
        }
      else // (zone>=1): pixel center in safety range
        {
        if (o<omax) // check sublevels
          for (int i=0; i<4; ++i) // add children in reverse order
            stk.push(4*pix+3-i,o+1); // add children
        else // at resolution limit
          {
          pixset.append(pix>>>(2*(o-order)));// output the parent pixel at order
          stk.popToMark(); // unwind the stack
          }
        }
      }
    else // o==order
      {
      if (zone>=2)
        pixset.append(pix);
      else if (inclusive) // and (zone>=1)
        {
        if (order<omax) // check sublevels
          {
          stk.mark(); // remember current stack position
          for (int i=0; i<4; ++i) // add children in reverse order
            stk.push(4*pix+3-i,o+1); // add children
          }
        else // at resolution limit
          pixset.append(pix); // output the pixel
        }
      }
    }

  /** Experimental; Do not use (yet) */
  private double[] pixel_boundaries(long i_th, long i_phi, int i_zone,
    double cos_theta)
    {
    double phi_l,phi_r;
    if (((i_th==1)||(i_th==nl4-1))&&(Math.abs(cos_theta)>=1-1/(3.0*npface))) {
      phi_l = 0;
      phi_r = 1;
    } else if (cos_theta >= Constants.twothird) {
      double sq3th = Math.sqrt(3*(1-cos_theta));
      double factor = 1/(nside*sq3th);
      double jd = i_phi;
      double ku = i_th - i_phi;
      phi_l = Math.max((jd-1)*factor, 1- (ku+1)*factor);
      phi_r = Math.min(1 - ku*factor, jd*factor);
    } else if (cos_theta >= -Constants.twothird) {
      double cth34 = 0.5 - 0.75*cos_theta;
      double cth34_1 = cth34 + 1.0;
      int modfactor = (int) (nside + ((i_th-nside)&1));
      double jd = i_phi - 0.5*(modfactor-i_th);
      double ku = 0.5*(modfactor+i_th) - i_phi;
      phi_l = Math.max( cth34_1 - (ku+1)/nside, -cth34 + (jd-1)/nside);
      phi_r = Math.min( cth34_1 - ku/nside, -cth34 + jd/nside);
    } else {
      double sq3th = Math.sqrt(3*(1+cos_theta));
      double factor = 1/(nside*sq3th);
      double jd = i_th - nl2 + i_phi;
      double ku = nl2 - i_phi;
      phi_l = Math.max(1 - (nl2-jd+1)*factor, (nl2-ku-1)*factor);
      phi_r = Math.min(1 - (nl2-jd)  *factor, (nl2-ku  )*factor);
    }
    double[] ret = { Constants.halfpi*(phi_l+i_zone),
                     Constants.halfpi*(phi_r+i_zone) };
    return ret;
  }
  /** Experimental; Do not use (yet) */
  private Vec3[] corners(long pix, int step) throws Exception
    {
    int nPoints = step * 2 + 2;
    Vec3[] points = new Vec3[nPoints];
    Zphi p0 = pix2zphi(pix);
    double cos_theta = p0.z;
    double theta = Math.acos(p0.z);
    double phi = p0.phi;

    int i_zone = (int)(phi/Constants.halfpi);
    long ringno = pix2ring(pix);
    long i_phi_count = Math.min(ringno, Math.min(nside, nl4-ringno));
    long i_phi = 0;
    double phifac = Constants.halfpi / i_phi_count;
    if ( ringno>=nside && ringno<=nl3 ) {
      i_phi = (long)(phi/phifac + 0.5*((ringno-nside)&1)) + 1;
    } else {
      i_phi = (long)(phi/phifac) + 1;
    }
    // adjust for zone offset
    i_phi -= i_zone*i_phi_count;
    int spoint = nPoints/2;
    // get north south middle - middle should match theta !
    double[] nms = { ring2z(ringno-1), cos_theta, ring2z(ringno+1) };
    double ntheta = Math.acos(nms[0]);
    double stheta = Math.acos(nms[2]);
    double[] philr = pixel_boundaries(ringno, i_phi, i_zone, nms[0]);
    if (i_phi > (i_phi_count/2)) {
      points[0] = new Vec3(new Pointing(ntheta, philr[1]));
    } else {
      points[0] = new Vec3(new Pointing(ntheta, philr[0]));
    }
    philr = pixel_boundaries(ringno, i_phi, i_zone, nms[2]);
    if ( i_phi > (i_phi_count/2) ) {
      points[spoint] = new Vec3(new Pointing(stheta, philr[1]));
    } else {
      points[spoint] = new Vec3(new Pointing(stheta, philr[0]));
    }
    if ( step == 1 ) {
      philr = pixel_boundaries(ringno, i_phi, i_zone, nms[1]);
      points[1] = new Vec3(new Pointing(theta, philr[0]));
      points[3] = new Vec3(new Pointing(theta, philr[1]));
    } else {
      double cosThetaLen = nms[2] - nms[0];
      double cosThetaStep = ( cosThetaLen / ( step + 1 ) );
      for ( int p = 1; p <= step; p++ ) {
        cos_theta = nms[0] + ( cosThetaStep * p );
        theta = Math.acos(cos_theta);
        philr = pixel_boundaries(ringno, i_phi, i_zone, cos_theta);
        points[p] = new Vec3(new Pointing(theta, philr[0]));
        points[nPoints-p] = new Vec3(new Pointing(theta, philr[1]));
      }
    }
    return points;
  }

  /** Compute ring index from pixel number.
      Works in both RING and NESTED schemes
      @param pix pixel number
      @return ring index (1 to 4Nside-1) */
  public long pix2ring (long pix)
    {
    if (scheme==Scheme.RING)
      {
      if (pix<ncap) // North Polar cap
        return (1+HealpixUtils.isqrt(1+2*pix))>>>1; // counted from North pole
      else if (pix<(npix-ncap)) // Equatorial region
        return (pix-ncap)/nl4 + nside; // counted from North pole
      else // South Polar cap
        return nl4-((1+HealpixUtils.isqrt(2*(npix-pix)-1))>>>1);
      }
    else
      {
      Xyf xyf = nest2xyf(pix);
      return ((long)(jrll[xyf.face])<<order) - xyf.ix - xyf.iy - 1;
      }
    }

  }
