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
 *  For more information about HEALPix, see http://healpix.sourceforge.net
 */

package healpix.essentials;
import java.util.ArrayList;

/** Support for MOC queries.
    @copyright 2015 Max-Planck-Society
    @author Martin Reinecke */
public class MocQuery
  {
  static public enum MocQueryOp { AND,OR,XOR,NOT,NONE }

  static public class MocQueryComponent
    {
    public MocQueryOp op;
    public Vec3 center;
    public double radius;
    public MocQueryComponent(MocQueryOp op_) throws Exception
      {
      op= op_;
      HealpixUtils.check(op_!=MocQueryOp.NONE,"bad operator");
      }
    public MocQueryComponent(Vec3 cnt, double rad)
      {
      op = MocQueryOp.NONE;
      center = new Vec3(cnt);
      center.normalize();
      radius = rad;
      }
    }

  static private class querulator
    {
    private final class pstack
      {
      private long[] p;
      private int[] o;
      private int s, m;

      public pstack (int sz)
        { p=new long[sz]; o=new int[sz]; s=m=0; }
      public void push (long p_, int o_)
        { p[s]=p_; o[s]=o_; ++s; }
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
      public void clear ()
        { s=m=0; }
      public boolean empty ()
        { return s==0; }
      }

    private int order, omax;
    private boolean inclusive;
    private ArrayList<MocQueryComponent> comp;
    private HealpixBase base[];
    private double cr[], crmin[][], crmax[][];

    private pstack stk; // stack for pixel numbers and their orders
    private long pix;
    private int o;
    private Vec3 pv;
    private int loc;

    private void check_pixel (int zone, Moc pixset)
      {
      if (zone==0) return;
      if (o<order)
        {
        if (zone>=3)
          pixset.addPixel(o,pix); // output all subpixels
        else // (zone>=1)
          for (int i=0; i<4; ++i)
            stk.push(4*pix+3-i,o+1); // add children
        }
      else if (o>order) // this implies that inclusive==true
        {
        if (zone>=2) // pixel center in shape
          {
          pixset.addPixel(order,pix>>>(2*(o-order))); // output parent pixel
          stk.popToMark(); // unwind the stack
          }
        else // (zone>=1): pixel center in safety range
          {
          if (o<omax) // check sublevels
            for (int i=0; i<4; ++i) // add children in reverse order
              stk.push(4*pix+3-i,o+1);
          else // at resolution limit
            {
            pixset.addPixel(order,pix>>>(2*(o-order))); // output parent pixel
            stk.popToMark(); // unwind the stack
            }
          }
        }
      else // o==order
        {
        if (zone>=2)
          pixset.addPixel(order,pix);
        else if (inclusive) // and (zone>=1)
          {
          if (order<omax) // check sublevels
            {
            stk.mark(); // remember current stack position
            for (int i=0; i<4; ++i) // add children in reverse order
              stk.push(4*pix+3-i,o+1);
            }
          else // at resolution limit
            pixset.addPixel(order,pix); // output the pixel
          }
        }
      }

    void correctLoc() throws Exception
      {
      int myloc=loc--;
      HealpixUtils.check((myloc>=0)&&(myloc<comp.size()),"inconsistency");
      switch (comp.get(myloc).op)
        {
        case AND:
        case OR:
        case XOR:
          correctLoc();
          correctLoc();
          break;
        case NOT:
          correctLoc();
          break;
        case NONE:
          break;
        }
      }
    int getZone (int zmin, int zmax) throws Exception
      {
      if (zmin==zmax) { correctLoc(); return zmin; } // short-circuit
      int myloc=loc--;
      HealpixUtils.check((myloc>=0)&&(myloc<comp.size()),"inconsistency");
      switch (comp.get(myloc).op)
        {
        case AND:
          {
          int z1=getZone(zmin,zmax);
          return getZone(zmin,z1);
          }
        case OR:
          {
          int z1=getZone(zmin,zmax);
          return getZone(z1,zmax);
          }
        case XOR:
          {
          int z1=getZone(0,3);
          int z2=getZone(0,3);
          return Math.max(zmin,Math.min(zmax,
            Math.max(Math.min(z1,3-z2),Math.min(3-z1,z2))));
          }
        case NOT:
          return 3-getZone(3-zmax,3-zmin);
        case NONE:
          {
          int res=zmax;
          double crad=pv.dot(comp.get(myloc).center);
          if (crad<=crmax[o][myloc]) res=0;
          else if (crad<=cr[myloc]) res=1;
          else if (crad<=crmin[o][myloc]) res=2;
          return Math.max(zmin,Math.min(zmax,res));
          }
        }
      HealpixUtils.check(false,"must not get here");
      return -1;
      }

    public querulator (int order_, int omax_, boolean inclusive_,
      ArrayList<MocQueryComponent> comp_) throws Exception
      {
      order=order_;
      omax=omax_;
      inclusive=inclusive_;
      comp=(ArrayList<MocQueryComponent>)comp_.clone();
      base=new HealpixBase[omax+1];
      cr=new double[comp.size()];
      crmin=new double [omax+1][comp.size()];
      crmax=new double [omax+1][comp.size()];
      HealpixUtils.check(comp.size()>=1,"bad query component ArrayList");
      HealpixUtils.check(order<=omax,"order>omax");
      if (!inclusive) HealpixUtils.check(order==omax,"inconsistency");
      HealpixUtils.check(omax<=HealpixBase.order_max,"omax too high");

      for (int i=0; i<comp.size(); ++i)
        if (comp.get(i).op==MocQueryOp.NONE) // it's a cap
          cr[i]=Math.cos(comp.get(i).radius);
      for (o=0; o<=omax; ++o) // prepare data at the required orders
        {
        base[o] = new HealpixBase(1<<o,Scheme.NESTED);
        double dr=base[o].maxPixrad(); // safety distance
        for (int i=0; i<comp.size(); ++i)
          if (comp.get(i).op==MocQueryOp.NONE) // it's a cap
            {
            crmax[o][i] = (comp.get(i).radius+dr>=Math.PI) ?
              -1.01 : Math.cos(comp.get(i).radius+dr);
            crmin[o][i] = (comp.get(i).radius-dr<=0.) ?
               1.01 : Math.cos(comp.get(i).radius-dr);
            }
        }
      stk=new pstack(12+3*omax); // reserve maximum size to avoid reallocation
      }
    Moc result() throws Exception
      {
      Moc pixset = new Moc();
      stk.clear();
      stk.mark();
      for (int i=0; i<12; ++i) // insert the 12 base pixels in reverse order
        stk.push(11-i,0);

      while (!stk.empty()) // as long as there are pixels on the stack
        {
        // pop current pixel number and order from the stack
        pix=stk.ptop();
        o=stk.otop();
        stk.pop();
        pv = base[o].pix2vec(pix);

        loc=comp.size()-1;
        int zone=getZone(0,3);
        check_pixel (zone, pixset);
        HealpixUtils.check(loc==-1,"stack not used up");
        }
      return pixset;
      }
    }

  static public Moc doMocQuery (int order, ArrayList<MocQueryComponent> comp)
    throws Exception
    {
    querulator quer=new querulator(order,order,false,comp);
    return quer.result();
    }

  static private boolean isEar(int i, ArrayList<Vec3> vv, ArrayList<Vec3> normal,
    ArrayList<Boolean> convex)
    {
    if (!convex.get(i)) return false;
    int nv=normal.size();
    int pred=(i+nv-1)%nv, succ=(i+1)%nv;
    Vec3 ab=new Vec3(normal.get(i)),
         bc=vv.get(succ).cross(vv.get(pred)).norm(),
         ca=new Vec3(normal.get(pred));
    for (int j=(succ+1)%nv; j!=pred; j=(j+1)%nv)
      if ((vv.get(j).dot(ab)>0)
        &&(vv.get(j).dot(bc)>0)
        &&(vv.get(j).dot(ca)>0))
        return false;
    return true;
    }

  // vertices are assumed to be normalized
  private static void processVertices(ArrayList<Vec3> vv, ArrayList<Vec3> normal,
    ArrayList<Boolean> convex, ArrayList<Boolean> ear) throws Exception
    {
    int nv=vv.size();
    do
      {
      nv=vv.size();
      for (int i=0; i<vv.size(); ++i)
        {
        int succ=(i+1)%vv.size();
        double v_ang = vv.get(i).angle(vv.get(succ));
        if (v_ang<1e-8) // vertices are very close
          {
          vv.remove(i);
          break;
          }
        if (v_ang>Math.PI-1e-8) // vertices almost antipodal
          {
          HealpixUtils.check(false,"degenerate polygon edge");
          break;
          }
        int pred=(i+vv.size()-1)%vv.size();
        Vec3 npred=vv.get(pred).cross(vv.get(i)),
             nsucc=vv.get(i).cross(vv.get(succ));
        double n_ang = npred.angle(nsucc);
        if (n_ang<1e-8) // vertices almost on a straight line
          {
          vv.remove(i);
          break;
          }
        if (n_ang>Math.PI-1e-8) // very thin polygon spike
          {
          vv.remove(i);
          break;
          }
        }
      } while (nv!=vv.size());

    normal.clear();
    convex.clear();
    ear.clear();
    for (int i=0; i<nv;++i)
      normal.add(vv.get(i).cross(vv.get((i+1)%nv)).norm());
    for (int i=0; i<nv;++i)
      convex.add(normal.get(i).dot(vv.get((i+nv-1)%nv))>0);
    for (int i=0; i<nv;++i)
      ear.add(isEar(i,vv,normal,convex));
    }

  static public ArrayList<MocQueryComponent> prepPolygon (ArrayList<Vec3> vertex)
    throws Exception
    {
    HealpixUtils.check(vertex.size()>=3,"not enough vertices in polygon");
    ArrayList<Vec3> vv = new ArrayList<Vec3>(vertex.size());
    for (int i=0; i<vertex.size(); ++i)
      vv.add(vertex.get(i).norm());
    ArrayList<Vec3> normal = new ArrayList<Vec3>();
    ArrayList<Boolean> convex = new ArrayList<Boolean>(), ear = new ArrayList<Boolean>();

    processVertices(vv,normal,convex,ear);
    ArrayList<MocQueryComponent> comp = new ArrayList<MocQueryComponent>();

    int nconvex=0;
    for (int i=0; i<vv.size(); ++i)
      if (convex.get(i)) ++nconvex;
    if (nconvex==vv.size()) // fully convex polygon
      {
      for (int i=0; i<vv.size(); ++i)
        comp.add(new MocQueryComponent(normal.get(i),0.5*Math.PI));
      for (int i=1; i<vv.size(); ++i)
        comp.add(new MocQueryComponent(MocQueryOp.AND));
      return comp;
      }
    if (nconvex==0) // complement of a fully convex polygon??
      {
      for (int i=0; i<vv.size(); ++i)
        comp.add(new MocQueryComponent(normal.get(i).flipped(),0.5*Math.PI));
      for (int i=1; i<vv.size(); ++i)
        comp.add(new MocQueryComponent(MocQueryOp.AND));
      comp.add(new MocQueryComponent(MocQueryOp.NOT));
      return comp;
      }

    int earcount=0;
    while (vv.size()>2) // try to clip ears
      {
      int nvlast=vv.size();
      for (int i=0; i<vv.size();++i)
        {
        int pred=(i+vv.size()-1)%vv.size(), succ=(i+1)%vv.size();

        if (ear.get(i))
          {
          comp.add(new MocQueryComponent(normal.get(i),0.5*Math.PI));
          comp.add(new MocQueryComponent(vv.get(succ).cross
            (vv.get(pred)).norm(),0.5*Math.PI));
          comp.add(new MocQueryComponent(MocQueryOp.AND));
          comp.add(new MocQueryComponent(normal.get(pred),0.5*Math.PI));
          comp.add(new MocQueryComponent(MocQueryOp.AND));
          ++earcount;
          vv.remove(i);
          processVertices(vv,normal,convex,ear);
          break;
          }
        }
      HealpixUtils.check(vv.size()<nvlast,"failed to clip an ear");
      }
    for (int i=0; i<earcount-1; ++i)
      comp.add(new MocQueryComponent(MocQueryOp.OR));
    return comp;
    }

  static public Moc queryGeneralPolygon (ArrayList<Vec3> vertex, int order)
    throws Exception
    { return doMocQuery (order, MocQuery.prepPolygon(vertex)); }
  }
