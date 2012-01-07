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
import java.util.NoSuchElementException;

/** Class for dealing with sets of integer ranges.
    Ranges are described by the first element and the one-past-last element.

    @copyright 2011, 2012 Max-Planck-Society
    @author Martin Reinecke */
public class RangeSet {

  public interface ValueIterator {
    boolean hasNext();
    long next();
    }

  /** Sorted list of entries. */
  protected long[] r;
  /** current size */
  protected int sz;

  public RangeSet() { this(8); }
  /** Construct new object with a given initial number of entries
      @param cap */
  public RangeSet(int cap)
    {
    if (cap<0) throw new IllegalArgumentException("capacity must be positive");
    r = new long[cap];
    sz=0;
    }
  public RangeSet(long[] data)
    {
    sz=data.length;
    r = new long[sz];
    System.arraycopy(data,0,r,0,sz);
    checkConsistency();
    }

  public void checkConsistency()
    {
    if ((sz&1)!=0)
      throw new IllegalArgumentException("invalid number of entries");
    for (int i=1; i<sz; ++i)
      if (r[i]<=r[i-1])
        throw new IllegalArgumentException("inconsistent entries");
    }

  /** Make sure the object can hold at least the given number of entries. */
  public void ensureCapacity(int cap)
    {
    if (r.length<cap) // grow the array if necessary
      {
      int goalSize = Math.max(2*r.length,cap);

      long[] rnew = new long[goalSize];
      System.arraycopy(r,0,rnew,0,r.length);
      r = rnew;
      }
    }

  /** Append a single-value range to the object.
      @param val - value to append */
  public void append(long val)
    { append(val,val+1); }

  /** Append a range to the object.
      @param a first long in range
      @param b one-after-last long in range */
  public void append (long a, long b)
    {
    if (a>=b) return;
    if ((sz>0) && (a<=r[sz-1]))
      {
      if (a<r[sz-2]) throw new IllegalArgumentException("bad append operation");
      if (b>r[sz-1]) r[sz-1]=b;
      return;
      }
    ensureCapacity(sz+2);

    r[sz] = a;
    r[sz+1] = b;
    sz+=2;
    }

  /** Append an entire range set to the object. */
  public void append (RangeSet other)
    {
    for (int i=0; i<other.sz; i+=2)
      append(other.r[i],other.r[i+1]);
    }

  /** @return number of added ranges so far*/
  public int size()
    { return sz>>>1; }

  public boolean empty()
    { return sz==0; }

  public long ivbegin(int iv)
    { return r[2*iv]; }
  public long ivend(int iv)
    { return r[2*iv+1]; }

  public void clear()
    { sz=0; }

  private long getv(int i)
    { return r[i]; }
  private void pushv(long v)
    {
    ensureCapacity(sz+1);
    r[sz]=v;
    ++sz;
    }

  private static void generalUnion (RangeSet a, RangeSet b,
    boolean flip_a, boolean flip_b, RangeSet c)
    {
    c.clear();
    boolean state_a=flip_a, state_b=flip_b, state_res=state_a||state_b;
    int ia=0, ea=a.sz, ib=0, eb=b.sz, out=0;
    boolean runa = ia!=ea, runb = ib!=eb;
    while(runa||runb)
      {
      boolean adv_a=false, adv_b=false;
      long val=0,va=0,vb=0;
      if (runa) va = a.getv(ia);
      if (runb) vb = b.getv(ib);
      if (runa && (!runb || (va<=vb))) { adv_a=true; val=va; }
      if (runb && (!runa || (vb<=va))) { adv_b=true; val=vb; }
      if (adv_a) { state_a=!state_a; ++ia; runa = ia!=ea; }
      if (adv_b) { state_b=!state_b; ++ib; runb = ib!=eb; }
      if ((state_a||state_b)!=state_res)
        { c.pushv(val); state_res = !state_res; }
      }
    }
  public void setToUnion (RangeSet a, RangeSet b)
    { generalUnion (a,b,false,false,this); }
  public void setToIntersection (RangeSet a, RangeSet b)
    { generalUnion (a,b,true,true,this); }
  public void setToDifference (RangeSet a, RangeSet b)
    { generalUnion (a,b,true,false,this); }
  public RangeSet union (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,false,false,res);
    return res;
    }
  public RangeSet intersection (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,true,true,res);
    return res;
    }
  public RangeSet difference (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,true,false,res);
    return res;
    }

  private int iiv (long val)
    {
    int count=sz, first=0;
    while (count>0)
      {
      int step=count>>>1, it = first+step;
      if (r[it]<=val)
        { first=++it; count-=step+1; }
      else
        count=step;
      }
    return first-1;
    }
  public boolean contains (long a)
    { return ((iiv(a)&1)==0); }
  public boolean containsAll (long a,long b)
    {
    int res=iiv(a);
    if ((res&1)!=0) return false;
    return (b<=r[res+1]);
    }
  public boolean containsAny (long a,long b)
    {
    int res=iiv(a);
    if ((res&1)==0) return true;
    if (res==sz-1) return false; // beyond the end of the set
    return (r[res+1]<b);
    }
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if ((obj==null) || (!(obj instanceof RangeSet)))
      return false;
    RangeSet other = (RangeSet) obj;
    if (other.sz!=sz) return false;
    for (int i=0; i<sz; ++i)
      if (other.r[i]!=r[i]) return false;
    return true;
    }
  public long nval() {
    long res = 0;
    for (int i=0; i<sz; i+=2)
      res+=r[i+1]-r[i];
    return res;
    }

  private void addRemove (long a, long b, int v)
    {
    int pos1=iiv(a), pos2=iiv(b);
    if ((pos1>=0) && (r[pos1]==a)) --pos1;
    // first to delete is at pos1+1; last is at pos2
    boolean insert_a = (pos1&1)==v;
    boolean insert_b = (pos2&1)==v;
    int rmstart=pos1+1+(insert_a ? 1 : 0);
    int rmend  =pos2-(insert_b?1:0);

    if (((rmend-rmstart)&1)==0)
      throw new IllegalArgumentException("cannot happen: "+rmstart+" "+rmend);

    if (insert_a && insert_b && (pos1+1>pos2)) // insert
      {
      ensureCapacity(sz+2);
      System.arraycopy(r,pos1+1,r,pos1+3,sz-pos1-1); // move to right
      r[pos1+1]=a;
      r[pos1+2]=b;
      sz+=2;
      }
    else
      {
      if (insert_a) r[pos1+1]=a;
      if (insert_b) r[pos2]=b;
      if (rmstart!=rmend+1)
        System.arraycopy(r,rmend+1,r,rmstart,sz-rmend-1); // move to left
      sz-=rmend-rmstart+1;
      }
    }

  public void intersect (long a, long b)
    {
    int pos1=iiv(a), pos2=iiv(b);
    if ((pos2>=0) && (r[pos2]==b)) --pos2;
    // delete all up to pos1 (inclusive); and starting from pos2+1
    boolean insert_a = (pos1&1)==0;
    boolean insert_b = (pos2&1)==0;

    // cut off end
    sz=pos2+1;
    if (insert_b) r[sz++]=b;

    // erase start
    if (insert_a) r[pos1--]=a;
    if (pos1>=0)
      System.arraycopy(r,pos1+1,r,0,sz-pos1-1); // move to left

    sz-=pos1+1;
    if ((sz&1)!=0)
      throw new IllegalArgumentException("cannot happen");
    }

  public void add (long a, long b)
    { addRemove(a,b,1); }
  public void remove (long a, long b)
    { addRemove(a,b,0); }

  public long[] toArray(){
    long[] res = new long[(int)nval()];
    int ofs=0;
    for (int i=0; i<sz; i+=2)
      for (long j=r[i]; j<r[i+1]; ++j)
        res[ofs++]=j;
    return res;
    }

  public String toString()
    {
    StringBuilder s = new StringBuilder();
    s.append("{ ");
    for (int i=0; i<sz; i+=2)
      {
      s.append("["+r[i]+";"+r[i+1]+"]");
      if (i<sz-2) s.append(",");
      }
    s.append(" }");
    return s.toString();
    }

  public ValueIterator valueIterator()
    {
    return new ValueIterator()
      {
      int pos = 0;
      long value = (sz>0) ? r[0] : 0;

      public boolean hasNext()
        { return (pos<sz); }

      public long next() {
        if (pos>sz)
          throw new NoSuchElementException();
        long ret = value;
        if (++value==r[pos+1])
          {
          pos+=2;
          if (pos<sz)
            value = r[pos];
          }
        return ret;
        }
      };
    }
  }
