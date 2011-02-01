/*
 * HEALPix Java code supported by the Gaia project.
 * Copyright (C) 2006-2011 Gaia Data Processing and Analysis Consortium
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

package healpix.core.base.set;

/**
 * An iterator over ranges which does not require object creation
 * <p>
 * !!Implementation must return sorted ranges in iterator!!
 */
public interface LongRangeIterator {

    /** move to next Range in iterator 
     * @return true if more ranges are in iterator, false if iterator reached end 
     */
    boolean moveToNext();
    
//    /**
//     * Skip values on RangeIterator until current first is >= to last. 
//     * Is typically faster then moveToNext() in cycle with condition (uses binary search).
//     * This goes only forward, no backward
//     * @param last 
//     * @return true if more ranges are in iterator, false if iterator reached end 
//     */
//    boolean skipTo(long last);

    /**
     * @return first item in current range (inclusive)
     * @throws java.util.NoSuchElementException if no more elements are found
     */
    long first();

    /**
     * @return last item in current range (inclusive)
     * @throws java.util.NoSuchElementException if no more elements are found
     */
    long last();

}