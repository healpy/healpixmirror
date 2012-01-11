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
package healpix.newcore.test;

import junit.framework.TestCase;
import java.util.Random;
import java.text.DecimalFormat;

import healpix.newcore.*;

/** @author Martin Reinecke */
public class HealpixBaseTest extends TestCase {

  static private final int nsamples=10000; // influences number of correctness tests
  static private final DecimalFormat form = new DecimalFormat("##0.00");

  private Zphi random_zphi(Random rng)
    { return new Zphi(2*rng.nextDouble()-1,2*Math.PI*rng.nextDouble()); }

  private Pointing random_dir(Random rng)
    { return new Pointing(Math.acos(2*rng.nextDouble()-1),2*Math.PI*rng.nextDouble()); }

  static private final int nsteps=100000; // influences number of performance tests

  public void test_perf_math()
    {
    final int ncomp=1000000;
    System.out.println("Rough performance test of math functions");
    long cnt=0;
    double dummy=0;
    double di=1./ncomp;
    long tstart = System.nanoTime();
    for (double i=0; i<1; i+=di)
       { dummy+=Math.sqrt(i); ++cnt; }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("sqrt: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=4*Math.PI/ncomp;
    tstart = System.nanoTime();
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=Math.sin(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("sin : " + form.format(cnt/time*1e-6) + "MOps/s");
    tstart = System.nanoTime();
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=Math.cos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("cos : " + form.format(cnt/time*1e-6) + "MOps/s");
    di=2./ncomp;
    tstart = System.nanoTime();
    for (double i=-1; i<1; i+=di)
      { dummy+=Math.acos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("acos: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    for (double i=-500; i<500; i+=di)
      { dummy+=Math.atan(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    di=1000./ncomp;
    System.out.println("atan: " + form.format(cnt/time*1e-6) + "MOps/s");
    tstart = System.nanoTime();
    for (double i=-500; i<500; i+=di)
      { dummy+=Math.atan2(i,2.9); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("atan2: " + form.format(cnt/time*1e-6) + "MOps/s");
    tstart = System.nanoTime();
    di=1000./ncomp;
    for (double i=0; i<1000; i+=di)
      { dummy+=Math.IEEEremainder(i,3.45); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("IEEEremainder: " + form.format(cnt/time*1e-6) + "MOps/s");
    }

  private double subtest_perf_neighbours(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        long nres[] = base.neighbours(pix);
        dummy+=nres[0];
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name + ": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_pix2ang(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        Pointing p=base.pix2ang(pix);
        dummy+=p.theta+p.phi;
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name + ": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_pix2vec(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        Vec3 v=base.pix2vec(pix);
        dummy+=v.x+v.y+v.z;
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name + ": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_pix2zphi(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        Zphi blah=base.pix2zphi(pix);
        dummy+=blah.z;
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name + ": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_ring2nest() throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,Scheme.RING);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        dummy+=base.ring2nest(pix);
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("ring2nest       : " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_nest2ring() throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,Scheme.RING);
      long dpix=Math.max(base.getNpix()/nsteps,1L);
      for (long pix=0; pix<base.getNpix(); pix+=dpix)
        {
        dummy+=base.nest2ring(pix);
        ++cnt;
        }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("nest2ring       : " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_ang2pix(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    double dth=Math.PI/Math.sqrt(nsteps);
    double dph=Constants.twopi/Math.sqrt(nsteps);
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      for (double theta=0; theta<Math.PI; theta+=dth)
        for (double phi=0; phi<2*Math.PI; phi+=dph)
          {
          dummy+=base.ang2pix(new Pointing(theta+1e-15*phi,phi));
          ++cnt;
          }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name+": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_zphi2pix(String name, Scheme scheme)
    throws Exception
    {
    long cnt=0;
    double dummy=0;
    long tstart = System.nanoTime();
    int omax=HealpixBase.order_max;
    double dz=2./Math.sqrt(nsteps);
    double dph=Constants.twopi/Math.sqrt(nsteps);
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,scheme);
      for (double z=-1; z<1; z+=dz)
        for (double phi=0; phi<Constants.twopi; phi+=dph)
          {
          dummy+=base.zphi2pix(new Zphi(z,phi));
          ++cnt;
          }
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name+": " + form.format(cnt/time*1e-6) + "MOps/s");
    return dummy;
    }

  private double subtest_perf_query_disc(String name,
    Scheme scheme) throws Exception
    {
    long cnt=0;
    double dummy=0;
    HealpixBase base = new HealpixBase (1024,scheme);
    long tstart = System.nanoTime();
    for (int m=0; m<1000; ++m)
      {
      RangeSet lrs=base.queryDisc(new Pointing(new Vec3(1,0,0)),Constants.halfpi/9.,false);
      dummy+=lrs.size();
      ++cnt;
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name+": " + form.format(cnt/time*1e-3) + "kOps/s");
    return dummy;
    }

  private double subtest_perf_query_polygon(String name,
    Scheme scheme) throws Exception
    {
    long cnt=0;
    double dummy=0;
    HealpixBase base = new HealpixBase (1024,scheme);
    Pointing[] corner = new Pointing[4];
    corner[0]=new Pointing(new Vec3(1,0.01,0.01));
    corner[1]=new Pointing(new Vec3(1,1,-0.3));
    corner[2]=new Pointing(new Vec3(0.01,1,0.01));
    corner[3]=new Pointing(new Vec3(0.01,0.01,1));
    long tstart = System.nanoTime();
    for (int m=0; m<1000; ++m)
      {
      RangeSet lrs=base.queryPolygon(corner,false);
      dummy+=lrs.size();
      ++cnt;
      }
    double time = 1e-9*(System.nanoTime()-tstart);
    System.out.println(name+": " + form.format(cnt/time*1e-3) + "kOps/s");
    return dummy;
    }

  public void test_perf() throws Exception
    {
    System.out.println("Performance tests of HealpixBase methods");
    double d=0;
    d+=subtest_perf_neighbours("neighbours(NEST)",Scheme.NESTED);
    d+=subtest_perf_neighbours("neighbours(RING)",Scheme.RING);
    d+=subtest_perf_pix2ang   ("pix2ang   (NEST)",Scheme.NESTED);
    d+=subtest_perf_pix2ang   ("pix2ang   (RING)",Scheme.RING);
    d+=subtest_perf_ang2pix   ("ang2pix   (NEST)",Scheme.NESTED);
    d+=subtest_perf_ang2pix   ("ang2pix   (RING)",Scheme.RING);
    d+=subtest_perf_pix2vec   ("pix2vec   (NEST)",Scheme.NESTED);
    d+=subtest_perf_pix2vec   ("pix2vec   (RING)",Scheme.RING);
    d+=subtest_perf_pix2zphi  ("pix2zphi  (NEST)",Scheme.NESTED);
    d+=subtest_perf_pix2zphi  ("pix2zphi  (RING)",Scheme.RING);
    d+=subtest_perf_zphi2pix  ("zphi2pix  (NEST)",Scheme.NESTED);
    d+=subtest_perf_zphi2pix  ("zphi2pix  (RING)",Scheme.RING);
    d+=subtest_perf_ring2nest();
    d+=subtest_perf_nest2ring();
    d+=subtest_perf_query_disc("disc      (NEST)",Scheme.NESTED);
    d+=subtest_perf_query_disc("disc      (RING)",Scheme.RING);
    d+=subtest_perf_query_polygon("polygon   (NEST)",Scheme.NESTED);
    d+=subtest_perf_query_polygon("polygon   (RING)",Scheme.RING);
    }

  public void test_accuracy()throws Exception
    {
    System.out.println("Testing accuracy near the poles");

    for (int order=0; order<=HealpixBase.order_max; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,Scheme.RING);
      assertTrue("incorrect pix2ang",base.pix2ang(1).theta>0.0);
      }
    }

  public void test_ringnestring()throws Exception
    {
    System.out.println("Testing identity ring2nest(nest2ring(i))==i");
    Random rng = new Random();

    for (int order=0; order<=HealpixBase.order_max; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,Scheme.RING);
      for (int m=0; m<nsamples; ++m)
        {
        long pix = (long)(rng.nextDouble()*base.getNpix());
        assertEquals ("ringnestring problem",
          base.ring2nest(base.nest2ring(pix)),pix);
        }
      }
    }

  public void test_pixzphipix()throws Exception
    {
    System.out.println("Testing identity pix2zphi(zphi2pix(i))==i");
    int omax=HealpixBase.order_max;
    Random rng = new Random();
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base1 = new HealpixBase (1L<<order,Scheme.RING),
                  base2 = new HealpixBase (1L<<order,Scheme.NESTED);
      for (int m=0; m<nsamples; ++m)
        {
        long pix = (long)(rng.nextDouble()*base1.getNpix());
        assertEquals ("pixzphipix problem",
          pix, base1.zphi2pix(base1.pix2zphi(pix)));
        assertEquals ("pixzphipix problem",
          pix, base2.zphi2pix(base2.pix2zphi(pix)));
        }
      }
    for (long nside=3; nside<(1L<<omax); nside+=nside/2+1)
      {
      HealpixBase base = new HealpixBase (nside,Scheme.RING);
      for (int m=0; m<nsamples; ++m)
        {
        long pix = (long)(rng.nextDouble()*base.getNpix());
        assertEquals ("pixzphipix problem",
          pix, base.zphi2pix(base.pix2zphi(pix)));
        }
      }
    }

  public void test_zphipixzphi() throws Exception
    {
    System.out.println
      ("Testing near-identity zphi2pix(pix2zphi(i))approx i");
    int omax=HealpixBase.order_max;
    Random rng = new Random();
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base1 = new HealpixBase (1L<<order,Scheme.RING),
                  base2 = new HealpixBase (1L<<order,Scheme.NESTED);
      double mincos = Math.min (Math.cos(base1.maxPixrad()),0.999999999999999);
      for (int m=0; m<nsamples; ++m)
        {
        Zphi zp1 = random_zphi(rng);
        Zphi zp2 = base1.pix2zphi(base1.zphi2pix(zp1));
        double cd1 = HealpixUtils.cosdist_zphi(zp1.z,zp1.phi,zp2.z,zp2.phi);
        assertTrue("zphipixzphi problem "+cd1+" "+mincos,cd1>=mincos);
        zp2 = base2.pix2zphi(base2.zphi2pix(zp1));
        cd1 = HealpixUtils.cosdist_zphi(zp1.z,zp1.phi,zp2.z,zp2.phi);
        assertTrue("zphipixzphi problem",cd1>=mincos);
        }
      }
    for (long nside=3; nside<(1L<<omax); nside+=nside/2+1)
      {
      HealpixBase base = new HealpixBase (nside,Scheme.RING);
      double mincos = Math.min (Math.cos(base.maxPixrad()),0.999999999999999);
      for (int m=0; m<nsamples; ++m)
        {
        Zphi zp1 = random_zphi(rng);
        Zphi zp2 = base.pix2zphi(base.zphi2pix(zp1));
        double cd1 = HealpixUtils.cosdist_zphi(zp1.z,zp1.phi,zp2.z,zp2.phi);
        assertTrue("zphipixzphi problem",cd1>=mincos);
        }
      }
    }

  public void test_neighbors() throws Exception
    {
    System.out.println("Testing neighbour function");
    int omax=HealpixBase.order_max;
    Random rng = new Random();
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase base1 = new HealpixBase (1L<<order,Scheme.RING),
                  base2 = new HealpixBase (1L<<order,Scheme.NESTED);
      double maxang = 2.01*base1.maxPixrad();
      for (int m=0; m<nsamples; ++m)
        {
        long pix = (long)(rng.nextDouble()*base1.getNpix());
        Vec3 v = base1.pix2vec(pix);
        long [] nb = base1.neighbours(pix);
        long [] nb2 = base2.neighbours(base2.ring2nest(pix));
        int nnb=0;
        for (int n=0; n<8; ++n)
          {
          if (nb[n]<0)
            assertTrue("neighbour problem 1",nb2[n]<0);
          else
            {
            ++nnb;
            assertEquals("neighbour problem 2",base1.ring2nest(nb[n]),nb2[n]);
            assertTrue("neighbour problem 3",base1.pix2vec(nb[n]).angle(v)<maxang);
            }
          }
        assertTrue("neighbour problem 4 "+order+" "+nnb,(nnb>=7)||((order==0)&&(nnb>=6)));
        }
      }
    for (long nside=3; nside<(1L<<omax); nside+=nside/2+1)
      {
      HealpixBase base = new HealpixBase (nside,Scheme.RING);
      double maxang = 2.01*base.maxPixrad();
      for (int m=0; m<nsamples; ++m)
        {
        long pix = (long)(rng.nextDouble()*base.getNpix());
        Vec3 v = base.pix2vec(pix);
        long[] nb = base.neighbours(pix);
        int nnb=0;
        for (int n=0; n<8; ++n)
          if (nb[n]>=0)
            {
            ++nnb;
            assertTrue("neighbour problem 5",base.pix2vec(nb[n]).angle(v)<maxang);
            }
        assertTrue("neighbour problem 6",nnb>=7);
        }
      }
    }
  public void test_query_disc_strict() throws Exception
    {
    System.out.println("Testing non-inclusive queryDisc()");
    Random rng = new Random();
    for (int order=0; order<=5; ++order)
      {
      HealpixBase base = new HealpixBase (1L<<order,Scheme.NESTED);
      boolean[] map = new boolean[(int)base.getNpix()];
      Vec3[] vmap = new Vec3[(int)base.getNpix()];
      for (int m=0; m<base.getNpix(); ++m)
        {
        map[m]=false;
        vmap[m]=base.pix2vec(m);
        }
      for (int m=0; m<nsamples; ++m)
        {
        Pointing ptg = random_dir (rng);
        double rad = Math.PI * rng.nextDouble();
        RangeSet rs = base.queryDisc(ptg,rad,false);
        Vec3 vptg = new Vec3(ptg);
        double cosrad=Math.cos(rad);
        for (int i=0; i<rs.size(); ++i)
          for (long j=rs.ivbegin(i); j<rs.ivend(i); ++j)
            map[(int)j]=true;
        for (int i=0; i<base.getNpix(); ++i)
          {
          boolean inside = vmap[i].dot(vptg)>cosrad;
          assertFalse ("query_disc_strict problem",inside^map[i]);
          }
        for (int i=0; i<rs.size(); ++i)
          for (long j=rs.ivbegin(i); j<rs.ivend(i); ++j)
            map[(int)j]=false;
        }
      }
    }

  public void test_query_disc() throws Exception
    {
    System.out.println("Testing queryDisc() empirically");
    int omax=17;
    Random rng = new Random();
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase rbase = new HealpixBase (1L<<order,Scheme.RING),
                  nbase = new HealpixBase (1L<<order,Scheme.NESTED);
      int niter=Math.max(1,Math.min(nsamples/1000,100000>>order));
      for (int m=0; m<niter; ++m)
        {
        Pointing ptg = random_dir (rng);
        double rad = Math.PI * rng.nextDouble();
        RangeSet rs = rbase.queryDisc(ptg,rad,false);
        long nval = rs.nval();
        rs = nbase.queryDisc(ptg,rad,false);
        assertEquals("queryDisc problem 1", nval,rs.nval());
        rs = rbase.queryDisc(ptg,rad,true);
        long nv1 = rs.nval();
        rs = nbase.queryDisc(ptg,rad,true);
        long nv2 = rs.nval();
        assertTrue("queryDisc problem 2", nv1>=nv2);
        assertTrue("queryDisc problem 3", nv2>=nval);
        }
      }
    }

  public void testQueryPolygon() throws Exception
    {
    System.out.println("Testing queryPolygon()");
    HealpixBase base = new HealpixBase(1024,Scheme.NESTED);
    Pointing[] corner = new Pointing[4];
    corner[0]=new Pointing(new Vec3(1,0.01,0.01));
    corner[1]=new Pointing(new Vec3(1,1,-0.3));
    corner[2]=new Pointing(new Vec3(0.01,1,0.01));
    corner[3]=new Pointing(new Vec3(0.01,0.01,1));
    RangeSet lrs=base.queryPolygon(corner,false);
    assertEquals("QueryPolygon problem",lrs.nval(),1696714);
    lrs=base.queryPolygon(corner,true);
    assertEquals("QueryPolygon problem",lrs.nval(),1700206);
    base = new HealpixBase(1024,Scheme.RING);
    lrs=base.queryPolygon(corner,false);
    assertEquals("QueryPolygon problem",lrs.nval(),1696714);
    lrs=base.queryPolygon(corner,true);
    assertEquals("QueryPolygon problem",lrs.nval(),1700206);
    }

  public void testQueryPolygon2() throws Exception
    {
    System.out.println("Testing queryPolygon() empirically");
    int omax=17;
    Random rng = new Random();
    for (int order=0; order<=omax; ++order)
      {
      HealpixBase rbase = new HealpixBase (1L<<order,Scheme.RING),
                  nbase = new HealpixBase (1L<<order,Scheme.NESTED);
      int niter=Math.max(1,Math.min(nsamples/1000,100000>>order));
      for (int m=0; m<niter; ++m)
        {
        Pointing[] corner = new Pointing[3];
        corner[0]=random_dir(rng);
        corner[1]=random_dir(rng);
        corner[2]=random_dir(rng);
        RangeSet rs = rbase.queryPolygon(corner,false);
        long nval = rs.nval();
        rs = nbase.queryPolygon(corner,false);
        assertEquals("queryPolygon problem 1", nval,rs.nval());
        rs = rbase.queryPolygon(corner,true);
        long nv1 = rs.nval();
        rs = nbase.queryPolygon(corner,true);
        long nv2 = rs.nval();
        assertTrue("queryPolygon problem 2", nv1>=nv2);
        assertTrue("queryPolygon problem 3", nv2>=nval);
        }
      }
    }


  public void test() throws Exception
    {
    int nside=256;
    HealpixBase base  = new HealpixBase(nside,Scheme.NESTED);
    HealpixBase base2 = new HealpixBase(nside,Scheme.RING);
    for (int i=0; i<12*nside*nside; ++i)
      {
      assertEquals ("pixel mismatch_nest",i,base.ang2pix(base.pix2ang(i)));
      assertEquals ("pixel mismatch_nest",i,base.vec2pix(base.pix2vec(i)));
      assertEquals ("pixel mismatch_ring",i,base2.ang2pix(base2.pix2ang(i)));
      assertEquals ("pixel mismatch_ring",i,base2.vec2pix(base2.pix2vec(i)));
      assertEquals ("pixel mismatch",i,base.ring2nest(base2.ang2pix(base.pix2ang(i))));
      assertEquals ("pixel mismatch_ringnestring",i,base.ring2nest(base.nest2ring(i)));
      }
    }
  }
