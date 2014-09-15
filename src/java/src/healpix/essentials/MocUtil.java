/*
 *  This file is part of Healpix Java.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
package healpix.essentials;

import java.io.FileOutputStream;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.BinaryTable;
import nom.tam.fits.BinaryTableHDU;
import nom.tam.fits.Fits;
import nom.tam.fits.FitsFactory;
import nom.tam.fits.Header;
import nom.tam.util.BufferedDataOutputStream;

/** Moc I/O routines.

    @copyright 2014 Max-Planck-Society
    @author Martin Reinecke */
public class MocUtil
  {
  public static Moc mocFromString(String in)
    {
    in=in.replaceAll("[,]+"," ");
    in=in.replaceAll("[\\[\\]{}\"]+","");
    in=in.replaceAll("\\s*[:/]\\s*","/ ");
    in=in.replaceAll("\\s*-\\s*","-");
    in=in.replaceAll("[\\s]+"," ");
    String[] tok = in.split(" ");
    RangeSet ru = new RangeSet();
    int order=0;
    long ofs=0;
    for (int i=0; i<tok.length; ++i)
      {
      if (tok[i].contains("/")) // new order
        {
        tok[i]=tok[i].split("/")[0];
        order=Integer.parseInt(tok[i]);
        ofs = 4*(1L<<(2*order));
        }
      else if (tok[i].contains("-")) // range of values
        {
        String [] lim=tok[i].split("-");
        ru.add(Long.parseLong(lim[0])+ofs,
               Long.parseLong(lim[1])+ofs+1);
        }
      else // single value
        ru.add(Long.parseLong(tok[i])+ofs);
      }
    Moc res = new Moc();
    res.fromUniq(ru);
    return res;
    }
  private static String mocToStringGeneral(Moc moc, boolean json)
    {
    RangeSet ru = moc.toUniq();
    String res = new String();
    if (json) res+="{";
    boolean firstOrder=true;
    int omax = moc.maxOrder();
    for (int o=0; o<=omax; ++o)
      {
      RangeSet rt = new RangeSet();
      long offset=4*(1L<<(2*o));
      rt.append(offset, 4*offset);
      rt=rt.intersection(ru);
      boolean prefix=false;
      if (!rt.isEmpty())
	{
        for (int iv=0; iv<rt.size(); ++iv)
          {
          long a=rt.ivbegin(iv)-offset,
               b=rt.ivend(iv)-offset;
          if (!prefix)
            {
            if (!firstOrder) res+= json ? ", " : " ";
            firstOrder=false;
            res+=json ? "\""+o+"\":[" : o+"/";
            prefix=true;
            }
          else
            res+=",";
          if (json)
	    {
            for (long i=a;i<b-1;++i)
            res+=i+",";
            res+=(b-1);
            }
          else
	    {
            res+=a;
            if (b>a+1) res+="-"+(b-1);
            }
          }
        }
      if (json&&prefix)
        res+="]";
      }
    if (json) res+="}";
    return res;
    }
  public static String mocToStringASCII (Moc moc)
    {
    return mocToStringGeneral(moc,false);
    }
  public static String mocToStringJSON(Moc moc)
    {
    return mocToStringGeneral(moc,true);
    }

  public static Moc mocFromFits(String filename) throws Exception
    {
    FitsFactory.setUseHierarch(true);
    FitsFactory.setUseAsciiTables(false);
    BasicHDU bhdu = (new Fits(filename)).getHDU(1);
    Header head = bhdu.getHeader();
    Header.setLongStringsEnabled(true);

    Object tmp = ((BinaryTable)bhdu.getData()).getFlattenedColumn(0);
    long[] data=null;
    if (tmp instanceof long[])
      data = (long[])tmp;
    else if (tmp instanceof int[])
      {
      int[] tmp2 = (int[])tmp;
      data = new long[tmp2.length];
      for (int i=0; i<tmp2.length; ++i)
        data[i] = (long)tmp2[i];
      }
    else if (tmp instanceof short[])
      {
      short[] tmp2 = (short[])tmp;
      data = new long[tmp2.length];
      for (int i=0; i<tmp2.length; ++i)
        data[i] = (long)tmp2[i];
      }
    else
      HealpixUtils.check(false,"unsupported data format");

    RangeSet ru = new RangeSet();
    for (int i=0; i<data.length; ++i)
      ru.append(data[i]);
    Moc moc = new Moc();
    moc.fromUniq(ru);
    return moc;
    }
  public static void mocToFits(Moc moc, String filename) throws Exception
    {
    FitsFactory.setUseHierarch(true);
    FitsFactory.setUseAsciiTables(false);
    Fits f = new Fits();
    Object[] table = new Object[1];
    long[] data=moc.toUniq().toArray();
    long maxval=0;
    if (data.length>0) maxval=data[data.length-1];
    if (maxval<=0x7fff)
      {
      short[] dtmp = new short[data.length];
      for (int i=0; i<data.length; ++i)
        dtmp[i]=(short)data[i];
      table[0]=dtmp;
      }
    else if (maxval<=0x7FFFFFFF)
      {
      int[] dtmp = new int[data.length];
      for (int i=0; i<data.length; ++i)
        dtmp[i]=(int)data[i];
      table[0]=dtmp;
      }
    else
      table[0] = data;

    f.addHDU(Fits.makeHDU(table));
    BinaryTableHDU bhdu = (BinaryTableHDU) f.getHDU(1);

    bhdu.setColumnName(0, "PIXEL", "");
    bhdu.addValue("PIXTYPE", "HEALPIX", "HEALPix magic value");
    bhdu.addValue("ORDERING", "NUNIQ", "NUNIQ coding method");
    bhdu.addValue("COORDSYS", "C", "mandated by MOC standard");
    bhdu.addValue("MOCORDER", moc.maxOrder(), "MOC resolution (best order)");

    FileOutputStream fos = new FileOutputStream(filename);
    BufferedDataOutputStream s = new BufferedDataOutputStream(fos);

    f.write(s);
    s.flush();
    s.close();
    }
  }
