
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/
package healpix.core.base.set;

import java.io.Serializable;
import java.util.BitSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.TreeMap;

/**
 *  In Java Collections this class corresponds to TreeSet of longs. Is sorted,
 *  and contains only unique values. But memory requirements are greatly
 *  optimized (1000x times)
 * <p>  
 *  This class represents bit array based sets of long values. When a
  *  bit in the underlying array is set, the value having the same
  *  number as the bit is contained in the array. This implies that
  *  bit sets cannot contain negative values.
  *  <p>
  *  To improve memory performance, bit array is based in slot by 1e6,
  *  so when one number is allocated at 1e10, only one slot of size 1e6 is consumed.
  * <p>
  *  This class have very good IO speed and good memory consumption. But
  * memory consumption is still linear, so for readonly sets LongRangeSet is better option
   * @author Jan Kotek
  */
public class LongSet extends LongCollection implements Serializable {

    private static final long serialVersionUID = 87858445112407625L;

    /**
     * size of slot
     */
    protected final int slotSize;
    
    public LongSet(){
    	this(1024);
    }

    public LongSet(int slotSize) {
		super();
		this.slotSize = slotSize;
	}

	/**
     * Slots. Key is slot offset, value is BitSet representing values in given slot
     * Because Long is object there is box and unbox overhead on each operation
     * TODO replace key, so does not need boxing  
     */
    protected final TreeMap<Long, BitSet> slots = new TreeMap<Long, BitSet>();
    
    protected long cachedKey = -1;
    protected BitSet cachedBitSet = null;
    
    protected BitSet findBitSet(long key){
    	if(key != cachedKey){
    		cachedKey = key;
    		cachedBitSet = slots.get(key);
    	}
    	return cachedBitSet;

    }
    protected BitSet findOrCreateBitSet(long key){
    	cachedBitSet = findBitSet(key);
    	if(cachedBitSet == null){
    		cachedBitSet = new BitSet(slotSize); 
    		slots.put(key,cachedBitSet);
    	}
   		return cachedBitSet;

    }


    public LongSet(LongCollection set) {
    	this();
        addAll(set);
    }


    public LongSet(long[] set) {
    	this();
        addAll(set);
    }

    public LongSet(LongIterator longIterator) {
    	this();
        addAll(longIterator);
    }

    public void add(long v){
        int slotVal = (int) (v % slotSize);
        long key = new Long(v - slotVal);
        BitSet ret = findOrCreateBitSet(key);
        ret.set((int) slotVal);
    }

    public void clear() {
        slots.clear();
        cachedBitSet = null;
        cachedKey = -1;
    }


    public boolean contains(long v) {
        int slotVal = (int) (v % slotSize);
        long key = new Long(v - slotVal);
        BitSet ret = findBitSet(key);

        return ret != null && ret.get(slotVal);
    }

    public LongIterator longIterator(){
        return new LongIterator(){

            /** iterator over offsets*/
            Iterator<Long> iter = slots.keySet().iterator();
            /** current offset */
            long offset = iter.hasNext()?iter.next():-1;            
            
            /** current bit set */
            BitSet curr = offset!=-1?slots.get(offset):null;
            /** position in current bit set */
            int pos = curr!=null?curr.nextSetBit(0): -1;

            public boolean hasNext() {
                return offset!=-1 && pos!=-1;
            }

            public long next() {
                if(offset==-1 || pos == -1) throw new NoSuchElementException();
                long ret = offset + pos;
                //move to next
                pos = curr.nextSetBit(pos+1);
                if(pos!=-1){
                	//do nothing, already moved to next
                }else if(pos == -1 && iter.hasNext()){
                    //move to next slot
                    offset = iter.next();
                    curr = slots.get(offset);
                    //find new starting position
                    pos = curr.nextSetBit(0);
                }else{
                    //no next slot, null everything
                    iter =null;
                    offset = -1;
                    curr = null;
                }

                return ret;
            }
        };
    }


    public int size() { //Requires linear traversal of set
        int size = 0;
        for(BitSet bs :slots.values()){
        	size+=bs.cardinality();
        }
        return size;
    }


    public void remove(long v) {
        int slotVal = (int) (v % slotSize);
        Long key = new Long(v - slotVal);
        BitSet slot = slots.get(key);
        if(slot == null) return;
        slot.set(slotVal,false);
        //cleanup if is last item in slot;
        if(slot.isEmpty()){
        	slots.remove(key);
        	if(slot == cachedBitSet){
        		cachedBitSet = null;
        		cachedKey = -1;
        	}
        		
        }
    }


     public boolean isEmpty(){
         return slots.isEmpty()||size()==0;
    }

    /** @return values in this set organized in readonly LongRangeSet */
    public LongRangeSet toLongRangeSet(){
        LongRangeSetBuilder b = new LongRangeSetBuilder();
        LongIterator i = longIterator();
        if(!i.hasNext())
            return b.build(); //empty set

        long oldVal = i.next();
        long rangeStart = oldVal;

        while(i.hasNext()){
            long val = i.next();
            if(oldVal + 1 != val){ //new range is starting
                b.appendRange(rangeStart,oldVal);
                rangeStart = val;
            }
            oldVal = val;
        }
        //add last item
        b.appendRange(rangeStart,oldVal);

        return b.build();
    }


}
