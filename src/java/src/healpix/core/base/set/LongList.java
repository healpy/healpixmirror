
/*
 * LongRangeSet from Jan Kotek redistributed under GPLv2
*/
package healpix.core.base.set;

import java.io.Serializable;
import java.util.Arrays;
import java.util.NoSuchElementException;

 

/**
 * Growable long[] array
   * @author Jan Kotek
 */
public class LongList extends LongCollection implements Serializable {

    private static final long serialVersionUID = -2794240565359961009L;

    protected int size = 0;
    protected long[] data = new long[32];
    

    public LongList(LongRangeSet longRangeSet) {
        addAll(longRangeSet.longIterator());
    }

    public void add(long v) {
        add(size,v);
    }

    public LongList() {
    }

    public LongList(LongCollection set) {
        addAll(set);
    }


    public LongList(long[] set) {
        addAll(set);
    }


	public void clear() {
        size = 0;
        data = new long[32];
    }

    public LongIterator longIterator() {
        return new LongIterator(){

            int pos = 0;
            public boolean hasNext() {
                return pos<size;
            }

            public long next(){
                if(pos>=size) throw new NoSuchElementException(); 
                pos++;
                return data[pos-1];
            }
        };
    }


    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public boolean contains(long v) {
        for (int i = 0; i < size; i++)
            if (data[i] == v)
                return true;
        return false;
    }

    public int indexOf(long c) {
        for (int i = 0; i < size; i++)
            if (data[i] == c)
                return i;
        return -1;
    }


    public void remove(long v) {
        int index = indexOf(v);
        if (index != -1) {
            removeElementAt(index);

        }
    }

    public void add(int index, long v) {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException();
        ensureCapacity(size+1);
        //  Move data
        int block = size-index;
        if (block > 0)
            System.arraycopy(data, index, data, index+1, block);
        data[index] = v;
        size++;
    }

    public long get(int index) {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException();
        return data[index];
    }

    public long set(int index, long v) {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException();
        long result = data[index];
        data[index] = v;
        return result;
    }

    public long removeElementAt(int index) {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException();
        long result = data[index];
        //  Move data
	    int block = size-(index+1);
        if (block > 0)
            System.arraycopy(data, index+1, data, index, block);
        size--;
        return result;
    }

    /**
     *  Ensures that this list has at least a specified capacity.
     *  The actual capacity is calculated from the growth factor
     *  or growth chunk specified to the constructor.
     *
     *  @param      capacity
     *              the minimum capacity of this list.
     *
     *  @return     the new capacity of this list.
     *
     */
    public int ensureCapacity(int capacity) {
        if (capacity > data.length) {
            capacity = Math.max(capacity, data.length * 2);
            long[] newdata = new long[capacity];
            System.arraycopy(data, 0, newdata, 0, size);
            data = newdata;
        }
        return capacity;
    }
    
    /** @return this array list, but sorted*/
    public LongList sort(){
        long[] data = toArray();
        Arrays.sort(data);
        return new LongList(data);
    }


    
}
