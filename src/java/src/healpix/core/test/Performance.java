package healpix.core.test;
import junit.framework.Assert;
import healpix.core.HealpixIndex;
import healpix.core.base.set.LongRangeSet;
import healpix.tools.SpatialVector;


/**  
 * measures performance
 */
public class Performance {

	/** helper class to print time */
	static public class StopWatch{
		
		long start = 0;
		public void start(){ 
			start = System.currentTimeMillis();
		}
		
		public void printTime(String label){ 
			long time  = System.currentTimeMillis() - start;
			System.out.println(label+" "+time+" ms");
		}

	}
	
	
	static StopWatch sw = new StopWatch();
	static HealpixIndex t = new HealpixIndex();
	static SpatialVector centre = new SpatialVector(1,1,1);
	static int nside = 0;
	static double radius = 0;
	static LongRangeSet result = null;

	public static void main(String[] args) throws Exception {
		centre.normalized();
		t= new HealpixIndex(HealpixIndex.calculateNSide(60)); //1' nside resolution
		nside = t.nside;
		radius = Math.toRadians(0.5);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("0.5 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");
	
		radius = Math.toRadians(10);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("10 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");

		t= new HealpixIndex(HealpixIndex.calculateNSide(1));
		nside = t.nside;
		radius = Math.toRadians(0.5);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("0.5 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");

		radius = Math.toRadians(10);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("10 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");

		t= new HealpixIndex(1048576);
		nside = t.nside;
		radius = Math.toRadians(0.5);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("0.5 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");

		radius = Math.toRadians(10);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("10 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");
		
		//TODO maximum Nside is 262144, but C++ maximum is order=29, nside = 536870912
		int nside1= 262144;//536870912;
		double res = HealpixIndex.getPixRes(nside1);
		System.out.println("Resolution for "+nside1+" is "+res);		
		nside = HealpixIndex.calculateNSide(res);//536870912; //highest res documented in C++ original code. Order=29
		Assert.assertTrue("Nside wrong ",nside == nside1);
		t= new HealpixIndex(HealpixIndex.calculateNSide(res));
		radius = Math.toRadians(0.5);
		sw.start();
		result = t.queryDisc(centre, radius, 0,1);
		sw.printTime("0.5 degrees at NSIDE="+nside+"  have "+result.size()+" pixels and took");
	}
}
