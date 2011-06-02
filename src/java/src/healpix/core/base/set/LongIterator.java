
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/
package healpix.core.base.set;

/**
 *  This class represents iterators over collections of long values.
 *  It returns primitive value, so there is no boxing overhead
 *
 *  @see        java.util.Iterator
 *  @author Jan Kotek
 */
public interface LongIterator {


    /**
     *  Indicates whether more long values can be returned by this
     *  iterator.
     *
     *  @return     <tt>true</tt> if more long values can be returned
     *              by this iterator; returns <tt>false</tt>
     *              otherwise.
     *
     *  @see        #next()
     */
    boolean hasNext();

    /**
     *  Returns the next long value of this iterator.
     *
     *  @return     the next long value of this iterator.
     *
     *  @throws java.util.NoSuchElementException
     *              if no more elements are available from this
     *              iterator.
     *
     *  @see        #hasNext()
     */
    long next();
    
}
