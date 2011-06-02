
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/

package healpix.core.base.set;

import java.util.Iterator;

/**
 * 
 * common methods for {@link LongSet} and {@link LongList} 
 * @author Jan Kotek
 */
public abstract class LongCollection implements Iterable<Long> {

    /**
     * Add value to set
     * @param v value to add
     */    
    public abstract void add(long v);

    /**
     * remove all iterms from set
     */
    public abstract void clear();

    /**
     * @param v value
     * @return true if set contains given value
     */
    public abstract boolean contains(long v);


    /**
     * add all values from given array
     * @param vals long array to add
     */
    public void addAll(long[] vals){
        for(long v :vals) add(v);
    }

	public void addAll(LongRangeSet set) {
		addAll(set.longIterator());		
	}

    /**
     * add all values from given iterator
     * @param iter LongIterator
     */
    public void addAll(LongIterator iter) {        
        while(iter.hasNext())
            add(iter.next());
   }

    /**
     * add all values from given set
     * @param set
     */
   public void addAll(LongCollection set) {
        addAll(set.longIterator());
   }

    /**
     * add all values from given range (inclusive)
     * @param first
     * @param last
     */
    public void addRange(long first, long last){
        for(long v = first;v<=last ; v++)
            add(v); //TODO sorted access can be optimized
    }

    /**
     * @return iterator LongIterator over values in this set with primitive long
     */
    
    public abstract LongIterator longIterator();

    public Iterator<Long> iterator() {
        return new Iterator<Long>(){

            LongIterator iter =longIterator();

            public boolean hasNext() {
                return iter.hasNext();
            }

            public Long next() {
                return iter.next();
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    /**
     * Count elements in set.
     * @return number of elements in this set*/    
    public abstract int size();

    /** remove given element from set
     * @param v to remove
     */    
    public abstract void remove(long v);

    /**
     * Return true if set is empty.
     * @return true if size is empty
     */
    public abstract boolean isEmpty();

    /**
     * @return  array of elements in collection
     */
    public long[] toArray(){
        long[] ret = new long[size()];
        LongIterator iter = longIterator();
        for(int i=0;iter.hasNext();i++)
            ret[i] = iter.next();
        return ret;
    }

    public String toString() {
    	StringBuilder s = new StringBuilder();
        s.append('[');
        LongIterator i = longIterator();
        while (i.hasNext()) {
            if (s.length() > 1)
                s.append(',');
            s.append(i.next());
        }
        s.append(']');
        return s.toString();
    }
    
    
    
    
}
