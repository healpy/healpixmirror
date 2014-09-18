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
  private final static int maxorder=29;
  private RangeSet rs;

  public Moc()
    { rs=new RangeSet(); }
  public Moc(Moc other)
    { rs=new RangeSet(other.rs); }

  static private Moc fromNewRangeSet(RangeSet rngset)
    {
    Moc res = new Moc();
    res.rs = rngset;
    return res;
    }

  public int maxOrder()
    {
    long combo=0;
    for (int i=0; i<rs.size(); ++i)
      combo|=rs.ivbegin(i)|rs.ivend(i);
    return maxorder-(Long.numberOfTrailingZeros(combo)>>>1);
    }

  public Moc degradedToOrder (int order, boolean keepPartialCells)
    {
    int shift=2*(maxorder-order);
    long ofs=(1L<<shift)-1;
    long mask = ~ofs;
    long adda = keepPartialCells ? 0L : ofs,
         addb = keepPartialCells ? ofs : 0L;
    RangeSet rs2=new RangeSet();
    for (int i=0; i<rs.size(); ++i)
      {
      long a=(rs.ivbegin(i)+adda)&mask;
      long b=(rs.ivend  (i)+addb)&mask;
      if (b>a) rs2.append(a,b);
      }
    return fromNewRangeSet(rs2);
    }

  public void addPixelRange (int order, long p1, long p2)
    {
    int shift=2*(maxorder-order);
    rs.add(p1<<shift,p2<<shift);
    }
  public Moc union (Moc other)
    { return fromNewRangeSet(rs.union(other.rs)); }
  public Moc intersection (Moc other)
    { return fromNewRangeSet(rs.intersection(other.rs)); }
  public Moc subtraction (Moc other)
    { return fromNewRangeSet(rs.difference(other.rs)); }
  public Moc complement()
    {
    RangeSet full = new RangeSet(new long[]{0L,12L*(1L<<(2*maxorder))});
    return fromNewRangeSet(full.difference(rs));
    }
  public boolean contains(Moc other) // FIXME: needs optimization!
    { return rs.contains(other.rs); }
  public boolean overlaps(Moc other) // FIXME: needs optimization!
    { return rs.overlaps(other.rs); }
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
  public static Moc fromUniq (RangeSet ru) // should be tuned!
    {
    RangeSet r= new RangeSet();
    RangeSet rtmp = new RangeSet();
    int lastorder=0;
    int shift=2*maxorder;
    for (int i=0; i<ru.size(); ++i)
      for (long j=ru.ivbegin(i); j<ru.ivend(i); ++j)
        {
        int order = HealpixUtils.uniq2order(j);
        if (order!=lastorder)
          {
          r=r.union(rtmp);
          rtmp.clear();
          lastorder=order;
          shift=2*(maxorder-order);
          }
        long pix = j-(1L<<(2*order+2));
        rtmp.append (pix<<shift,(pix+1)<<shift);
        }
    r=r.union(rtmp);
    return fromNewRangeSet(r);
    }

  public byte[] toByteArray() throws Exception
    {
    return rs.toByteArray();
    }
  public static Moc fromByteArray(byte[] data) throws Exception
    {
    return fromNewRangeSet(RangeSet.fromByteArray(data));
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