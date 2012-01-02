
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/

package healpix.core.base.set;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * This class represents range based sets of long values.
 * First and last boundaries are stored in sorted long[] array so it consumes very little place
 * This is essentially equivilent to run length encoding of a bitmap-based
 * set and thus works very well for sets with long runs of integers, but is
 * quite poor for sets with very short runs.
 * <p>
 * Is readonly, so is thread safe.
 * <p> 
 * To construct new LongRangeSet use {@link LongRangeSetBuilder} if 
 * our values are sorted. Or use {@link LongSet} if our values are unsorted.  
 * <p>
 * Inspired by Justin F. Chapweske and Soren Bak
 * @author Jan Kotek
 */
public class LongRangeSet implements Externalizable, Iterable<Long>{

    private static final long serialVersionUID = -7543399451387806240L;

    /** sorted ranges, even is first, odd is last */
    protected long[] ranges;
    
    /** empty constructor for serialization*/
    public LongRangeSet() {
    
	}

    /** 
     * Construct new LongRangeSet from given values
     * <p>
     * Dont use directly, use {@link LongRangeSetBuilder} instead
     *<p>
     * This constructor makes integrity check and copyes values into new array. 
     * When LongRangeSet is constructed inside LongRangeSet this operation can be 
     * skipped and set can be constructed way faster. 
     *  
     */
    public LongRangeSet(long[] values, int size){
    	if(size == 0){
    		ranges = new long[0];
    		return;
    	}
    		
    	if(size%2!=0) 
    		throw new IllegalArgumentException("not divide by 2");
        if(size<1)
            throw new IllegalArgumentException("et least one range is needed");

        if(values.length<size)
            throw new IllegalArgumentException("too small array");
        
        ranges = new long[size];

        long oldLast = Long.MIN_VALUE;
        for(int i = 0;i<size/2;i++){
            long first = values[i*2];
            long last = values[i*2+1];
            //check that is sorted
            if(first>last)
                throw new IllegalArgumentException("first > last");
            if(oldLast+1>=first && first!=Long.MIN_VALUE)
                throw new IllegalArgumentException("values are not sorted at oldLast: "+oldLast+", first: "+first);
            //move to new array
            ranges[i*2] = first;
            ranges[i*2+1] = last;
        }
    }

    /** 
     * @return Iterator over all longs in this set
     */
    public LongIterator longIterator() {
    	if(isEmpty()) 
    		return new LongIterator(){
				public boolean hasNext() {				
					return false;
				}

				public long next() {
					throw new NoSuchElementException();
				}};
				
        return new LongIterator(){

                int pos = 0;
                long value = first();

                public boolean hasNext() {
                    return pos <ranges.length && value<=ranges[pos+1];
                }

                public long next() {
                    if(pos >= ranges.length || value>ranges[pos+1])
                        throw new NoSuchElementException();
                    long ret = value;
                    //move to next position
                    if(value<ranges[pos+1]) //move to next value
                        value++;
                    else{
                        pos+=2; //move to next range
                        if(pos<ranges.length)
                            value = ranges[pos];
                    }
                    return ret;
                }
            };
        }


    /** 
     * @return LongRangeIterator over ranges in this set
     */
    public LongRangeIterator rangeIterator(){
    	if(isEmpty()) return new LongRangeIterator(){

			public long first() {
				throw new NoSuchElementException();
			}

			public long last() {
				throw new NoSuchElementException();			
			}

			public boolean moveToNext() {			
				return false;
			}

};    	
    	
        return new LongRangeIterator(){

            int pos = -2;

            public boolean moveToNext() {
                pos+=2;
                return pos<ranges.length;
            }

            public long first() {
            	if(pos<0)
            		throw new IllegalAccessError("Call moveToNext() first");
                if(pos>=ranges.length)
                    throw new NoSuchElementException();
                return ranges[pos];
            }

            public long last() {
            	if(pos<0)
            		throw new IllegalAccessError("Call moveToNext() first");
            	
                if(pos>=ranges.length)
                    throw new NoSuchElementException();
                return ranges[pos+1];
            }

        };
    }

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
     * @return first element in set
     * @throws NoSuchElementException if set isEmpty()
     */
    public long first() {
    	if(isEmpty())
    		throw new NoSuchElementException();
        return ranges[0];
    }

    /**
     * @return last element in set
     * @throws NoSuchElementException if set isEmpty() 
     */
    public long last() {
    	if(isEmpty())
    		throw new NoSuchElementException();
        return ranges[ranges.length-1];
    }

    /**
     * @param i The integer to check to see if it is in this set..
     * @return true if i is in the set.
     */
    public boolean contains(long i) {
        int pos = Arrays.binarySearch(ranges,i);
        if (pos > 0) {
            return true;
        }
        pos = -(pos+1);
        return pos % 2 != 0;
    }
    
    public boolean containsAll(long first, long last){
        if(first>last)
            throw new IllegalArgumentException("First is bigger then last");
        if(isEmpty() || last < first() || first>last())
            return false;

        int index = Arrays.binarySearch(ranges, first);
        if(index<0)
            index = -index-1;

        index = index - index%2;

        return ranges[index]<=first && ranges[index+1]>=last;
    }

    public boolean containsAny(long first, long last){
    	if(first>last)
    		throw new IllegalArgumentException("First is bigger then last");
    	if(isEmpty() || last < first() || first>last())
    		return false;

    	int firstIndex = Arrays.binarySearch(ranges, first);    	
    	int lastIndex = Arrays.binarySearch(ranges, last);

    	
    	//System.out.println(firstIndex + " - "+lastIndex);
    	
    	
    	if(firstIndex <0 && lastIndex<0 && firstIndex%2==-1 && firstIndex == lastIndex)
    		return false;


    	return true;
    }
    

    
    public boolean containsAll(LongIterator iter){
    	while(iter.hasNext())
    		if(!contains(iter.next()))
    			return false;
    	return true;
    }
    
    public boolean containsAny(LongIterator iter){
    	while(iter.hasNext())
    		if(contains(iter.next()))
    			return true;
    	return false;
    }

    public boolean containsAll(LongRangeIterator iter){
    	while(iter.moveToNext())
    		if(!containsAll(iter.first(), iter.last()))
    			return false;
    	return true;
    }
    
    public boolean containsAny(LongRangeIterator iter){
    	while(iter.moveToNext())
    		if(containsAny(iter.first(), iter.last()))
    			return true;
    	return false;
    }

    /** 
     * Converts to LongSet which can be modified 
     * @return LongSet 
     */
    public LongSet toLongSet() {
        LongSet ret = new LongSet();
        ret.addAll(longIterator());
        return ret;
    }

    /** 
     * @return number of longs (pixels) in this set. !!NOT number of ranges!!
     */
    public long size() {
        long size = 0;
        LongRangeIterator iter = rangeIterator();
        while(iter.moveToNext()){
            size+=1 + iter.last() - iter.first();
        }
        return size;
    }
    
    /** 
     * @return number of ranges in this set
     */
    public int rangeCount(){
    	return ranges.length/2;    	
    }


    /**
     * Convert all items in range set to array.
     * With large set, this method will fail with OutOfMemoryException
     * @return  array of elements in collection
     */
    public long[] toArray(){
        long[] ret = new long[(int)size()];
        LongIterator iter = longIterator();
        for(int i=0;iter.hasNext();i++)
            ret[i] = iter.next();
        return ret;
    }

    
    public String toString() {
        StringBuilder s = new StringBuilder();       
        s.append('[');
        LongRangeIterator iter = rangeIterator();
        while(iter.moveToNext()){
            if (s.length() > 1)
                s.append(',');
            s.append(iter.first());
            s.append('-');
            s.append(iter.last());
        }
        s.append(']');
        return s.toString();
    }
       
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(ranges);
		return result;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof LongRangeSet))
			return false;
		LongRangeSet other = (LongRangeSet) obj;
		if (!Arrays.equals(ranges, other.ranges))
			return false;
		return true;
	}

	/**
	 * Create new LongRangeSet with complement (inversion) of values in this set. 
	 * Bounds of complement  are Long.MIN_VALUE and Long.MAX_VALUE
	 * if operation is called twice, original set is produced 
	 * <p>
	 * This operation is FAST. Requires only one traversal of ranges. 
	 * It does not decompress RangeSet to pixels.  
	 * <p>
	 * This operation does not modify original collection.
	 * 
	 * @return inverted LongRangeSet 
	 */
	public LongRangeSet complement() {
		LongRangeSetBuilder b = new LongRangeSetBuilder(ranges.length +2);
		long last = Long.MIN_VALUE;
		LongRangeIterator iter = rangeIterator();
		while(iter.moveToNext()){
			if(iter.first() != Long.MIN_VALUE) //was already starting with Long.MIN_VALUE, ignore first range
				b.appendRange(last, iter.first()-1);
			last = iter.last()+1;
		}		
		
		//close
		if(//case with completely empty set
				last==Long.MIN_VALUE  && b.size()==0 ||
		  //case when last was not Long.MAX_VALUE (there is +1 overflow) and close prev value		
				last!=Long.MIN_VALUE  && b.size()>0 )
			b.appendRange(last,Long.MAX_VALUE);
		return b.build();
	}
    
	
    /**
     * Create new LongRangeSet which contains union of values from 
     * original set and parameter
	 * <p>
	 * This operation is FAST. Requires only one traversal of ranges. 
	 * It does not decompress RangeSet to pixels.  
 	 * <p>
	 * This operation does not modify original collection.
	 *  
     * 
     * @param rs LongRangeSet to make union with
     * @return LongRangeSet contains union of original set and parameter set
     */
    public LongRangeSet union(LongRangeSet rs) {
    	LongRangeIterator it1 = rangeIterator();
    	LongRangeIterator it2 = rs.rangeIterator();

    	LongRangeSetBuilder rsb = new LongRangeSetBuilder();
    	
    	//boolean indicates if iterator have more data
    	boolean run1 = it1.moveToNext();
    	boolean run2 = it2.moveToNext();
    	
    	//problem is that data appended in builder must be sorted
    	//so use two iterators at the same time and produce sorted result
    	
    	while(run1 || run2){ //repeat until any of iterators have data    		
    		if(run1 && (!run2 || it1.last()<it2.first())){
        		//scroll first iterator until it overlaps    			
    			rsb.appendRange(it1.first(), it1.last());
    			run1 = it1.moveToNext();
    		}else if(run2 && (!run1 || it2.last()<it1.first())) {    		
    			//	scroll second iterator until it overlaps    		    				
    			rsb.appendRange(it2.first(), it2.last());
    			run2 = it2.moveToNext();
    		}else if(run1 && run2){
    			//overlap
    			long minFirst = Math.min(it1.first(), it2.first());    				
    			long maxLast = Math.max(it1.last(), it2.last());
    			rsb.appendRange(minFirst, maxLast);
    			run1 = it1.moveToNext();
    			run2 = it2.moveToNext();
    		}else{
    			//should not be here
    			throw new InternalError();
    		}
    	}
    	
    	return rsb.build();
//    	//naive implementation for tests
//    	LongSet b = new LongSet();
//    	b.addAll(longIterator());
//    	b.addAll(rs.longIterator());
//    	return b.toLongRangeSet();
    }

    /**
     * Construct new LongRangeSet with intersection of values from original set and 
     * parameter set. 
     * <p>
	 * This operation is FAST. Requires only one traversal of ranges. 
	 * It does not decompress RangeSet to pixels.  
 	 * <p>
	 * This operation does not modify original collection.

     * @param rs The set with which to intersect with this set.
     * @return new set that represents the intersect of original and parameter set
     */
    public LongRangeSet intersect(LongRangeSet rs) {
    	if(isEmpty())
    		return rs;
    	if(rs.isEmpty())
    		return this;
    	if(first()>rs.last() || last()<rs.first())
    		return LongRangeSetBuilder.EMPTY;

    	LongRangeIterator it1 = rangeIterator();
    	LongRangeIterator it2 = rs.rangeIterator();

    	LongRangeSetBuilder rsb = new LongRangeSetBuilder();
    	
    	//boolean indicates if iterator have more data
    	boolean run1 = it1.moveToNext();
    	boolean run2 = it2.moveToNext();
    	
    	//problem is that data appended in builder must be sorted
    	//so use two iterators at the same time and produce sorted result
    	
    	while(run1 && run2){ //repeat until both iterators have data
//    		System.out.println(it1.first() + " - "+it1.last());
//    		System.out.println(it2.first() + " - "+it2.last());
//    		System.out.println();
    		if(it1.last()<it2.first()){
            	//scroll first iterator until it overlaps    			
        		run1 = it1.moveToNext(); //TODO use binary search to jump to next value here
        	}else if(it2.last()<it1.first()) {    		
        		//	scroll second iterator until it overlaps
       			run2 = it2.moveToNext(); //TODO use binary search to jump to next value here
    		}else if(it1.first()<it2.first() && it1.last()>it2.last()){
    			//second range inside first
    			rsb.appendRange(it2.first(), it2.last());
    			run2 = it2.moveToNext();
    		}else if(it2.first()<it1.first() && it2.last()>it1.last()){
    			//first range inside first
    			rsb.appendRange(it1.first(), it1.last());
    			run1 = it1.moveToNext();
    		}else {
    			//overlap
    			long maxFirst = Math.max(it1.first(), it2.first());    				
    			long minLast = Math.min(it1.last(), it2.last());
    			rsb.appendRange(maxFirst, minLast);
    			if(it1.last()<it2.last())
    				run1 = it1.moveToNext();
    			else
    				run2 = it2.moveToNext();
   			}
    	}
    	
    	return rsb.build();
    	
//    	//simple implementation for testing
//        LongSet b = new LongSet();
//        LongIterator iter = longIterator();
//        while(iter.hasNext()){
//        	long next = iter.next();
//        	if(rs.contains(next))
//        		b.add(next);
//        }
//        return b.toLongRangeSet();
    }
    
    /**
     * 
     * Construct new LongRangeSet with values which are in original set, but not in parameter.
     * <p>
     * [1-5].substract[4-6] == [1-3] 
     * <p>
	 * This operation is FAST. Requires only one traversal of ranges. 
	 * It does not decompress RangeSet to pixels.  

 	 * <p>
	 * This operation does not modify original collection.
 
     * <p> substract this set from original
     * @return result of substraction
     */
    public LongRangeSet substract(LongRangeSet rs){
    	//quick check on boundaries
    	if(isEmpty() || rs.isEmpty() || first()>rs.last() || last()<rs.first())
    		return this;
    	
    	return intersect(rs.complement());

//    	//naive implementation for testing
//        LongSet b = new LongSet();
//        LongIterator iter = longIterator();
//        while(iter.hasNext()){
//        	long next = iter.next();
//        	if(!rs.contains(next))
//        		b.add(next);
//        }
//        return b.toLongRangeSet();
    	
    }

    /** 
     * @return true if set does not have any ranges
     */
	public boolean isEmpty() { 
		return  ranges.length==0;
	}

	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		ranges = LongRangeSetBuilder.readFrom(in).ranges;
	}

	public void writeExternal(ObjectOutput out) throws IOException {
		LongRangeSetBuilder.writeTo(out, this);
	}
}
