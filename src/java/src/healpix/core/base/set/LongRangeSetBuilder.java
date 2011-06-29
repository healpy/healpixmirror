
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/

package healpix.core.base.set;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

/**
 * Builder for LongRangeSet . LongRangeSet is unmodifiable, this class is 
 * 'factory' to create new instances.
 * <P>
 * To keep it fast and simple, this fab only supports appending. Uour ranges must be already 
 * sorted. This work  for most of Healpix based operations.
 * <p>
 * LongRangeSet can also be constructed using {@link LongSet}    
  * @author Jan Kotek
 */
public class LongRangeSetBuilder {

	/**
	 * empty LongRangeSet
	 */
	public static final LongRangeSet EMPTY = new LongRangeSet(new long[0],0);
	
	/** sorted list of ranges.*/
    protected long[] ranges ;
    /** current position*/
    protected int pos = 0;
    
    public LongRangeSetBuilder(){
    	this(32);
    }

    /**
     * construct new builder with given array size
     * @param arraySize
     */
    public LongRangeSetBuilder(int arraySize) {
    	if(arraySize%2!=0) throw new IllegalArgumentException("not divide by 2");
    	ranges = new long[arraySize]; 
	}

    /** make sure underling array have at least given size*/ 
	public void ensureSize(int arraySize){
		if(arraySize%2!=0) throw new IllegalArgumentException("not divide by 2");
        // grow the array if necessary.
        if (ranges.length  < arraySize) {
            long[] newRanges = new long[arraySize];
            System.arraycopy(ranges,0,newRanges,0,ranges.length);
            ranges = newRanges;
        }
    }


	/** append single long into builder 
	 * @param first - long to append
	 */
    public void append(long first){
        appendRange(first,first);
    }

    /**
     * append range into builder
     * @param first long in range (inclusive)
     * @param last long in range(inclusive)
     */
    public void appendRange(long first, long last){
        if(first>last)
            throw new IllegalArgumentException("first > last");
        if(pos>0){
            if(twoOrBigger() && first<=lastLast())
                throw new IllegalArgumentException("Could not merge, ranges must be added sorted! lastLast:"+lastLast()+", newFirst:"+first);
            //Check if new range overlaps with last one.
            //In this case update last range, instead of adding new one
            if(first <=last()+1){
                ranges[pos-2] = Math.min(first,lastFirst());
                ranges[pos-1] = Math.max(last,last());
                return;
            }
        }



        //make sure there is space
        if(pos + 2>ranges.length)
            ensureSize(ranges.length * 2);

        //insert
        ranges[pos] = first;
        ranges[pos+1] = last;
        pos+=2;
    }

    protected long last() {
        return ranges[pos-1];
    }


        protected boolean twoOrBigger() {
            return pos > 3;
        }

        protected long lastLast() {
            return ranges[pos-3];
        }




    protected long lastFirst() {
        return ranges[pos-2];
    }

    /**
     * appends all ranges from iterator
     * @param iter LongRangeIterator
     */
    public void appendRanges(LongRangeIterator iter){
        while(iter.moveToNext())
            appendRange(iter.first(),iter.last());
    }
    
    /**
     * append all ranges from given LongRangeSet
     * @param set LongRangeSet to append
     */
	public void appendRangeSet(LongRangeSet set) {
		appendRanges(set.rangeIterator());		
	}

    /** @return number of added ranges so far*/
    public int size(){
        return pos/2;
    }

    /**
     * Construct new LongRangeSet from appended values *
     * @return LongRangeSet with appended values
     */
    public LongRangeSet build(){
    	if(pos == 0)
    		return EMPTY;
        return new LongRangeSet(ranges,pos);
    }
    
    /**
     * Write LongRangeSet into stream in an space efficient way.
     * Delta compression is used and Longs are stored in packed form. 
     * @param out
     * @param rs
     * @throws IOException
     */
    public static void writeTo(DataOutput out,LongRangeSet rs) throws IOException{
    	out.writeInt(rs.ranges.length);
    	long last = 0;
    	for(long i:rs.ranges){
    		//write packed differences between values, this way it ocupies less space
    		long diff = i - last;
    		LongPacker.packLong(out, diff);
    		last = i;
    	}
    	
    }
    
    /**
     * Read LongRangeSet from an input stream
     * @param in
     * @return the new LongRangeSet
     * @throws IOException
     */
    public static LongRangeSet readFrom(DataInput in) throws IOException{
    	int size = in.readInt();
    	long[] arr = new long[size];
    	long last = 0;
    	for(int i =0; i<size;i++){
    		long v = last + LongPacker.unpackLong(in);
    		arr[i] = v;
    		last = v;
    	}
    	return new LongRangeSet(arr,size);
    }


}
