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

/** Support for MOC algorithms.

    @copyright 2014 Max-Planck-Society
    @author Martin Reinecke */
public class Moc
  {
  private final int maxorder=29;
  private RangeSet rs;

  public Moc()
    { rs=new RangeSet(); }
  public Moc(Moc other)
    { rs=new RangeSet(other.rs); }

  public int maxOrder()
    {
    long combo=0;
    for (int i=0; i<rs.size(); ++i)
      combo|=rs.ivbegin(i)|rs.ivend(i);
    return maxorder-(Long.numberOfTrailingZeros(combo)>>>1);
    }

  public void degradeToOrder (int order, boolean keepPartialCells)
    {
    int shift=2*(maxorder-order);
    long ofs=(1L<<shift)-1;
    RangeSet rs2=new RangeSet();
    for (int i=0; i<rs.size(); ++i)
      {
      long a=rs.ivbegin(i), b=rs.ivend(i);
      if (keepPartialCells)
        b+=ofs;
      else
        a+=ofs;
      a=(a>>>shift)<<shift;
      b=(b>>>shift)<<shift;
      if (b>a) rs2.append(a,b);
      }
    rs=rs2;
    }

  public void addPixelRange (int order, long p1, long p2)
    {
    int shift=2*(maxorder-order);
    rs.add(p1<<shift,p2<<shift);
    }
  public void merge (Moc other)
    { rs = rs.union(other.rs); }
  public void intersect (Moc other)
    { rs = rs.intersection(other.rs); }
  public void subtract (Moc other)
    { rs = rs.difference(other.rs); }
  public void invert()
    {
    RangeSet full = new RangeSet();
    full.append(0,12*(1L<<(2*maxorder)));
    rs = full.difference(rs);
    }
  public boolean containsAll(Moc other)
    { return rs.containsAll(other.rs); }
  public boolean containsAny(Moc other)
    { return rs.containsAny(other.rs); }
  public RangeSet toUniq() // should be tuned!
    {
    RangeSet r2 = new RangeSet(rs);
    RangeSet r3 = new RangeSet();
    RangeSet res= new RangeSet();
    for (int o=0; o<=maxorder; ++o)
      {
      if (r2.isEmpty()) return res;

      int shift = 2*(maxorder-o);
      long ofs=(1L<<shift)-1;
      long ofs2 = 1L<<(2*o+2);
      r3.clear();
      for (int iv=0; iv<r2.size(); ++iv)
        {
        long a=(r2.ivbegin(iv)+ofs)>>>shift,
             b=r2.ivend(iv)>>>shift;
        r3.append(a<<shift, b<<shift);
        res.append(a+ofs2,b+ofs2);
        }
      if (!r3.isEmpty())
        r2 = r2.difference(r3);
      }
    return res;
    }
  public void fromUniq (RangeSet ru) // should be tuned!
    {
    rs.clear();
    RangeSet rtmp = new RangeSet();
    int lastorder=0;
    int shift=2*maxorder;
    for (int i=0; i<ru.size(); ++i)
      for (long j=ru.ivbegin(i); j<ru.ivend(i); ++j)
        {
        int order = HealpixUtils.uniq2order(j);
        if (order!=lastorder)
          {
          rs=rs.union(rtmp);
          rtmp.clear();
          lastorder=order;
          shift=2*(maxorder-order);
          }
        long pix = j-(1L<<(2*order+2));
        rtmp.append (pix<<shift,(pix+1)<<shift);
        }
    rs=rs.union(rtmp);
    }

  public byte[] toByteArray() throws Exception
    {
    return rs.toByteArray();
    }
  public void fromByteArray(byte[] data) throws Exception
    {
    rs.fromByteArray(data);
    }

  public boolean equals(Object obj)
    {
    if (this == obj)
      return true;
    if ((obj==null) || (!(obj instanceof Moc)))
      return false;
    Moc other = (Moc) obj;
    return rs.equals(other.rs);
    }
  }
