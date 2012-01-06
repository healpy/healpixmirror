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

package healpix.core;

import java.util.Arrays;

/** Functionality related to the HEALPix pixelisation.
    This class is conceptually very similar the the Healpix_Map<double> class
    of Healpix_cxx.

    Copyright (C) 2011 Max-Planck-Society
    @author Martin Reinecke */
public class HealpixMapDouble extends HealpixBase
  {
  private double[] data;

  public static final double undef=-1.6375e30;

  public HealpixMapDouble()
    {
    super();
    data=null;
    }
  public HealpixMapDouble(long nside_in, Scheme scheme_in) throws Exception
    {
    super(nside_in,scheme_in);
    data=new double[(int)getNpix()];
    }
  public HealpixMapDouble(double[] data_in, Scheme scheme_in) throws Exception
    {
    super(npix2Nside(data_in.length),scheme_in);
    data=data_in;
    }

  /** Adjusts the object to nside_in.
      @param nside_in the new Nside parameter */
  public void setNside (long nside_in) throws Exception
    {
    if (nside_in!=nside)
      {
      super.setNside(nside_in);
      data=new double[(int)getNpix()];
      }
    }

  /** Adjusts the object to nside_in and scheme_in.
      @param nside_in the new Nside parameter
      @param scheme_in the new ordering scheme */
  public void setNsideAndScheme (long nside_in, Scheme scheme_in)
    throws Exception
    {
    super.setNsideAndScheme(nside_in,scheme_in);
    data=new double[(int)getNpix()];
    }

  public void setDataAndScheme(double[] data_in, Scheme scheme_in)
    throws Exception
    {
    super.setNsideAndScheme(npix2Nside(data_in.length),scheme_in);
    data=data_in;
    }

  public void fill(double val)
    { Arrays.fill(data,val); }

  public void swapScheme() throws Exception
    {
    HealpixUtils.check((order>=0) && (order<=13),
      "swapping not supported for this Nside");
    for (int m=0; m<swap_cycle[order].length; ++m)
      {
      int istart = swap_cycle[order][m];

      double pixbuf = data[(int)istart];
      long iold = istart,
           inew = (scheme==Scheme.RING) ? nest2ring(istart) : ring2nest(istart);
      while (inew != istart)
        {
        data[(int)iold] = data[(int)inew];
        iold = inew;
        inew = (scheme==Scheme.RING) ? nest2ring(inew) : ring2nest(inew);
        }
      data[(int)iold] = pixbuf;
      }
    scheme = (scheme==Scheme.RING) ? Scheme.NESTED : Scheme.RING;
    }

  public double getPixel(int ipix)
    { return data[ipix]; }
  public double getPixel(long ipix)
    { return data[(int)ipix]; }
  public void setPixel(int ipix, double val)
    { data[ipix] = val; }
  public void setPixel(long ipix, double val)
    { data[(int)ipix] = val; }

  public double[] getData()
    { return data; }

  public void importNograde (HealpixMapDouble orig) throws Exception
    {
    HealpixUtils.check (nside==orig.nside,
      "importNograde: maps have different nside");
    if (orig.scheme == scheme)
      for (int m=0; m<npix; ++m) data[m] = orig.data[m];
    else
      for (int m=0; m<npix; ++m)
        data[scheme==Scheme.NESTED ? (int)ring2nest(m) : (int)nest2ring(m)]
          = orig.data[m];
    }
  public void importUpgrade (HealpixMapDouble orig) throws Exception
    {
    HealpixUtils.check(nside>orig.nside,"importUpgrade: this is no upgrade");
    int fact = (int)(nside/orig.nside);
    HealpixUtils.check(nside==orig.nside*fact,
      "the larger Nside must be a multiple of the smaller one");

    for (int m=0; m<orig.npix; ++m)
      {
      Xyf xyf = orig.pix2xyf(m);
      int x=xyf.ix, y=xyf.iy, f=xyf.face;
      for (int j=fact*y; j<fact*(y+1); ++j)
        for (int i=fact*x; i<fact*(x+1); ++i)
          {
          long mypix = xyf2pix(i,j,f);
          data[(int)mypix] = orig.data[m];
          }
      }
    }
  public void importDegrade (HealpixMapDouble orig, boolean pessimistic)
    throws Exception
    {
    HealpixUtils.check(nside<orig.nside,"importDegrade: this is no degrade");
    int fact = (int)(orig.nside/nside);
    HealpixUtils.check(orig.nside==nside*fact,
      "the larger Nside must be a multiple of the smaller one");

    int minhits = pessimistic ? fact : 1;
    for (int m=0; m<npix; ++m)
      {
      Xyf xyf = pix2xyf(m);
      int x=xyf.ix, y=xyf.iy, f=xyf.face;
      int hits = 0;
      double sum = 0;
      for (int j=fact*y; j<fact*(y+1); ++j)
        for (int i=fact*x; i<fact*(x+1); ++i)
          {
          int opix = (int)orig.xyf2pix(i,j,f);
          if (!HealpixUtils.approx(orig.data[opix],undef,1e-5))
            {
            ++hits;
            sum += orig.data[opix];
            }
          }
      data[m] = (hits<minhits) ? undef : (double) (sum/hits);
      }
    }
  public void Import (HealpixMapDouble orig, boolean pessimistic)
    throws Exception
    {
    if (orig.nside==nside)
      importNograde(orig);
    else if (orig.nside<nside) // upgrading
      importUpgrade(orig);
    else
      importDegrade(orig,pessimistic);
    }
  }
