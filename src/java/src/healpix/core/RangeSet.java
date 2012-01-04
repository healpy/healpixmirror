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
import java.util.NoSuchElementException;

/** Class for dealing with sets of integer ranges.
    Ranges are described by the first element and the one-past-last element.
    @author Martin Reinecke */
public class RangeSet {

  public interface ValueIterator {
    boolean hasNext();
    long next();
    }

  /** Sorted list of ranges. */
  protected long[] r;
  /** current size */
  protected int sz = 0;

  public RangeSet() { this(16); }
  /** Construct new object with a given initial capacity (number of ranges)
      @param cap */
  public RangeSet(int cap)
    {
    if (cap<1) throw new IllegalArgumentException("capacity too small");
    r = new long[2*cap];
    }

  /** Make sure the object can hold at least the given number of ranges. */
  public void ensureCapacity(int cap)
    {
    // grow the array if necessary.
    if (r.length < 2*cap)
      {
      long[] rnew = new long[2*cap];
      System.arraycopy(r,0,rnew,0,r.length);
      r = rnew;
      }
    }

  /** Append a single-value range to the object.
      @param val - value to append */
  public void append(long val)
    {
    if ((sz>0) && (val<=r[sz-1]))
      {
      if (val<r[sz-2]) throw new IllegalArgumentException("bad append operation");
      if (val==r[sz-1]) ++r[sz-1];
      return;
      }
    if (sz+2>r.length)
      ensureCapacity(r.length);

    r[sz] = val;
    r[sz+1] = val+1;
    sz+=2;
    }

  /** Append a range to the object.
      @param a long in range (inclusive)
      @param b long in range (exclusive) */
  public void append (long a, long b)
    {
    if (a>=b) return;
    if ((sz>0) && (a<=r[sz-1]))
      {
      if (a<r[sz-2]) throw new IllegalArgumentException("bad append operation");
      if (b>r[sz-1]) r[sz-1]=b;
      return;
      }
    if (sz+2>r.length)
      ensureCapacity(r.length);

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
    if (sz+1>r.length)
      ensureCapacity(r.length);
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
    s.append('{');
    for (int i=0; i<sz; i+=2)
      {
      s.append(r[i]);
      if (r[i+1]>r[i]+1)
        s.append("-"+(r[i+1]-1));
      if (i<sz-2) s.append(",");
      }
    s.append('}');
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
