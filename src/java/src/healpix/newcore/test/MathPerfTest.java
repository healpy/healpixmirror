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
import java.text.DecimalFormat;

import healpix.newcore.*;

/** @author Martin Reinecke */
public class MathPerfTest extends TestCase
  {
  static private final DecimalFormat form = new DecimalFormat("##0.00");
  final int ncomp=10000000;

  public void test_perf_math()
    {
    System.out.println("Rough performance test of math functions");
    long cnt, tstart;
    double dummy=0, di, time;
    di=1./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=0; i<1; i+=di)
       { dummy+=Math.sqrt(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("sqrt: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=4*Math.PI/ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=Math.sin(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("sin : " + form.format(cnt/time*1e-6) + "MOps/s");
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=Math.cos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("cos : " + form.format(cnt/time*1e-6) + "MOps/s");
    di=2./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-1; i<1; i+=di)
      { dummy+=Math.acos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("acos: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-500; i<500; i+=di)
      { dummy+=Math.atan(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("atan: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-500; i<500; i+=di)
      { dummy+=Math.atan2(i,2.9); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("atan2: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=0; i<1000; i+=di)
      { dummy+=HealpixUtils.fmodulo(i,3.45); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("IEEEremainder: " + form.format(cnt/time*1e-6) + "MOps/s");
    }
  public void test_perf_fastmath()
    {
    System.out.println("Rough performance test of fast math functions");
    long cnt,tstart;
    double dummy=0, di, time;
    di=4*Math.PI/ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=FastMath.sin(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("sin : " + form.format(cnt/time*1e-6) + "MOps/s");
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-2*Math.PI; i<2*Math.PI; i+=di)
      { dummy+=FastMath.cos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("cos : " + form.format(cnt/time*1e-6) + "MOps/s");
    di=2./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-1; i<1; i+=di)
      { dummy+=FastMath.acos(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("acos: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-500; i<500; i+=di)
      { dummy+=FastMath.atan(i); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("atan: " + form.format(cnt/time*1e-6) + "MOps/s");
    di=1000./ncomp;
    tstart = System.nanoTime();
    cnt=0;
    for (double i=-500; i<500; i+=di)
      { dummy+=FastMath.atan2(i,2.9); ++cnt; }
    time = 1e-9*(System.nanoTime()-tstart);
    System.out.println("atan2: " + form.format(cnt/time*1e-6) + "MOps/s");
    }
  }
