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
import java.io.*;
import java.util.NoSuchElementException;

/** Class for dealing with sets of integer ranges.
    Ranges are described by the first element and the one-past-last element.
    This code was inspired by Jan Kotek's "LongRangeSet" class, but has been
    completely reimplemented.

    @copyright 2011, 2012 Max-Planck-Society
    @author Martin Reinecke */
public class RangeSet implements Externalizable
  {
  private static final long serialVersionUID = -4778909346616313978L;

  /** Interface describing an iterator for going through all values in
      a RangeSet object. */
  public interface ValueIterator {
    public boolean hasNext();
    public long next();
    }

  private static final ValueIterator EMPTY_ITER = new ValueIterator()
    {
    public boolean hasNext() { return false; }
    public long next() { throw new NoSuchElementException(); }
    };

  /** Sorted list of entries. */
  protected long[] r;
  /** Current number of active entries. */
  protected int sz;

  /** Construct new object with space for 8 entries (4 ranges). */
  public RangeSet() { this(8); }
  /** Construct new object with a given initial number of entries.
      @param cap number of initially reserved entries. */
  public RangeSet(int cap)
    {
    if (cap<0) throw new IllegalArgumentException("capacity must be positive");
    r = new long[cap];
    sz=0;
    }
  /** Construct new object from an array of longs.
      @param data */
  public RangeSet(long[] data)
    {
    sz=data.length;
    r = new long[sz];
    System.arraycopy(data,0,r,0,sz);
    checkConsistency();
    }
  /** Construct new object from another RangeSet
      @param other */
  public RangeSet(RangeSet other)
    {
    sz=other.sz;
    r = new long[sz];
    System.arraycopy(other.r,0,r,0,sz);
    }

  public void setTo(RangeSet other)
    {
    sz=other.sz;
    r = new long[sz];
    System.arraycopy(other.r,0,r,0,sz);
    }

  /** Checks the object for internal consistency. If a problem is detected,
      an IllegalArgumentException is thrown. */
  public void checkConsistency()
    {
    if ((sz&1)!=0)
      throw new IllegalArgumentException("invalid number of entries");
    for (int i=1; i<sz; ++i)
      if (r[i]<=r[i-1])
        throw new IllegalArgumentException("inconsistent entries");
    }

  private void resize(int newsize)
    {
    if (newsize<sz)
      throw new IllegalArgumentException("requested array size too small");
    if (newsize==r.length) return;
    long[] rnew = new long[newsize];
    System.arraycopy(r,0,rnew,0,sz);
    r = rnew;
    }

  /** Make sure the object can hold at least the given number of entries. */
  public void ensureCapacity(int cap)
    { if (r.length<cap) resize (Math.max(2*r.length,cap)); }
  /** Shrinks the array for the entries to minimum size. */
  public void trimSize()
    { resize(sz); }

  /** Append a single-value range to the object.
      @param val value to append */
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

  /** @return number of ranges in the set. */
  public int size()
    { return sz>>>1; }

  /** @return true if no entries are stored, else false. */
  public boolean isEmpty()
    { return sz==0; }

  /** @return first number in range iv. */
  public long ivbegin(int iv)
    { return r[2*iv]; }
  /** @return one-past-last number in range iv. */
  public long ivend(int iv)
    { return r[2*iv+1]; }

  /** Remove all entries in the set. */
  public void clear()
    { sz=0; }

  /** Push a single entry at the end of the entry vector. */
  private void pushv(long v)
    { ensureCapacity(sz+1); r[sz++]=v; }

  private static boolean generalAllOrNothing (RangeSet a, RangeSet b,
    boolean flip_a, boolean flip_b)
    {
    if (a.isEmpty())
      return flip_a ? true : b.isEmpty();
    if (b.isEmpty())
      return flip_b ? true : a.isEmpty();
    boolean state_a=flip_a, state_b=flip_b, state_res=state_a||state_b;
    int ia=0, ea=a.sz, ib=0, eb=b.sz;
    boolean runa = ia!=ea, runb = ib!=eb;
    while(runa||runb)
      {
      boolean adv_a=false, adv_b=false;
      long va=0,vb=0;
      if (runa) va = a.r[ia];
      if (runb) vb = b.r[ib];
      if (runa && (!runb || (va<=vb))) adv_a=true;
      if (runb && (!runa || (vb<=va))) adv_b=true;
      if (adv_a) { state_a=!state_a; ++ia; runa = ia!=ea; }
      if (adv_b) { state_b=!state_b; ++ib; runb = ib!=eb; }
      if ((state_a||state_b)!=state_res)
        return false;
      }
    return true;
    }
  /** Internal helper function for constructing unions, intersections
      and differences of two RangeSets. */
  private static void generalUnion (RangeSet a, RangeSet b,
    boolean flip_a, boolean flip_b, RangeSet c)
    {
    c.clear();
    if (a.isEmpty())
      {
      if (flip_a) return; // full range
      c.setTo(b); // only b
      return;
      }
    if (b.isEmpty())
      {
      if (flip_b) return; // full range
      c.setTo(a); // only a
      return;
      }
    boolean state_a=flip_a, state_b=flip_b, state_res=state_a||state_b;
    int ia=0, ea=a.sz, ib=0, eb=b.sz;
    boolean runa = ia!=ea, runb = ib!=eb;
    while(runa||runb)
      {
      boolean adv_a=false, adv_b=false;
      long val=0,va=0,vb=0;
      if (runa) va = a.r[ia];
      if (runb) vb = b.r[ib];
      if (runa && (!runb || (va<=vb))) { adv_a=true; val=va; }
      if (runb && (!runa || (vb<=va))) { adv_b=true; val=vb; }
      if (adv_a) { state_a=!state_a; ++ia; runa = ia!=ea; }
      if (adv_b) { state_b=!state_b; ++ib; runb = ib!=eb; }
      if ((state_a||state_b)!=state_res)
        { c.pushv(val); state_res = !state_res; }
      }
    }
  /** After this operation, the RangeSet contains the union of RangeSets a
      and b. */
  public void setToUnion (RangeSet a, RangeSet b)
    { generalUnion (a,b,false,false,this); }
  /** After this operation, the RangeSet contains the intersection of RangeSets
      a and b. */
  public void setToIntersection (RangeSet a, RangeSet b)
    { generalUnion (a,b,true,true,this); }
  /** After this operation, the RangeSet contains the difference of RangeSets
      a and b. */
  public void setToDifference (RangeSet a, RangeSet b)
    { generalUnion (a,b,true,false,this); }
  /** After this operation, the RangeSet contains the union of itself and
      other. */
  public RangeSet union (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,false,false,res);
    return res;
    }
  /** After this operation, the RangeSet contains the intersection of itself and
      other. */
  public RangeSet intersection (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,true,true,res);
    return res;
    }
  /** After this operation, the RangeSet contains the difference of itself and
      other. */
  public RangeSet difference (RangeSet other)
    {
    RangeSet res=new RangeSet();
    generalUnion (this,other,true,false,res);
    return res;
    }

  /** Returns an internal representation of the interval a number belongs to.
      @param val number whose interval is requested
      @return interval number, starting with -1 (smaller
      than all numbers in the RangeSet, 0 (first "on" interval), 2 (first
      "off" interval etc.) */
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
  /** Returns true if a is contained in the set, else false. */
  public boolean contains (long a)
    { return ((iiv(a)&1)==0); }
  /** Returns true if all numbers [a;b[ are contained in the set, else false. */
  public boolean containsAll (long a,long b)
    {
    int res=iiv(a);
    if ((res&1)!=0) return false;
    return (b<=r[res+1]);
    }
  /** Returns true if any of the numbers [a;b[ are contained in the set,
      else false. */
  public boolean containsAny (long a,long b)
    {
    int res=iiv(a);
    if ((res&1)==0) return true;
    if (res==sz-1) return false; // beyond the end of the set
    return (r[res+1]<b);
    }
  /** Returns true if the set completely contains "other", else false. */
  public boolean containsAll (RangeSet other)
    { return generalAllOrNothing(this,other,false,true); }
  /** Returns true if there is overlap between the set and "other",
      else false. */
  public boolean containsAny (RangeSet other)
    { return !generalAllOrNothing(this,other,true,true); }
  /** Returns true the object represents an identical set of ranges as obj. */
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

  public int hashCode()
    {
    int result = 0;
    for (int i=0;i<sz;i++)
      {
      int elementHash = (int)(r[i] ^ (r[i] >>> 32));
      result = 31 * result + elementHash;
      }
    result = 31 * result + sz;
    return result;
    }

  /** @return total number of values (not ranges) in the set. */
  public long nval()
    {
    long res = 0;
    for (int i=0; i<sz; i+=2)
      res+=r[i+1]-r[i];
    return res;
    }

  /** Internal helper function for building unions and differences of the
      RangeSet with a single range. */
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

  /** After this operation, the RangeSet contains the intersection of itself and
      [a;b[. */
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

  /** After this operation, the RangeSet contains the union of itself and
      [a;b[. */
  public void add (long a, long b)
    { addRemove(a,b,1); }
  /** After this operation, the RangeSet contains the difference of itself and
      [a;b[. */
  public void remove (long a, long b)
    { addRemove(a,b,0); }

  /** Creates an array cointainig all the numbers in the RangeSet.
      Not recommended, because the arrays can become prohibitively large.
      It is preferrable to use a ValueIterator or explicit loops. */
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
          s.append("[").append(r[i]).append(";").append(r[i + 1]).append("]");
          if (i<sz-2) s.append(",");
      }
    s.append(" }");
    return s.toString();
    }

  /** Returns a ValueIterator, which iterates over all individual numbers
      in the RangeSet. */
  public ValueIterator valueIterator()
    {
    if(sz == 0) return EMPTY_ITER;
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

  public void writeExternal(ObjectOutput out) throws IOException
    {
    out.writeInt(sz);
    long last = 0;
    for (long i:r)
      {
//write packed differences between values, this way it occupies less space
      long diff = i - last;
      packLong(out, diff);
      last = i;
      }
    }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
    {
    if(sz>0) throw new IOException("RangeSet already contains data!");
    sz = in.readInt();
    if((sz<0)||((sz&1)!=0)) throw new Error();
    r = new long[sz];
    long last = 0;
    for (int i =0; i<sz; i++)
      r[i] = last = last + unpackLong(in);
    }

  /** Pack non-negative long value into output stream.
    * It will occupy 1-9 bytes depending on value (lower values occupy
    * less space)
    *
    * @param os
    * @param value
    * @throws IOException
    *
    * Originally developed for Kryo by Nathan Sweet.
    * Modified for JDBM by Jan Kotek
    */
  static private void packLong(DataOutput os, long value) throws IOException
    {
    while ((value & ~0x7FL) != 0)
      {
      os.write((((int) value & 0x7F) | 0x80));
      value >>>= 7;
      }
    os.write((byte) value);
    }

  /** Unpack positive long value from the input stream.
    *
    * @param is The input stream.
    * @return The long value.
    * @throws java.io.IOException
    *
    * Originally developed for Kryo by Nathan Sweet.
    * Modified for JDBM by Jan Kotek
    */
  static private long unpackLong(DataInput is) throws IOException
    {
    long result = 0;
    for (int offset=0; offset<64; offset+=7)
      {
      long b = is.readUnsignedByte();
      result |= (b & 0x7F) << offset;
      if ((b & 0x80) == 0)
        return result;
      }
    throw new Error("Malformed long.");
    }
  }
