package minim.tester;

import se.sics.jasper.SICStus;
import se.sics.jasper.SPPredicate;
import se.sics.jasper.SPQuery;
import se.sics.jasper.SPTerm;

public class Tester {
	public static void main(String argv[]) {
		testMinim(argv);
	}

	// [(20-1), (30-1), (40-2), (10-1), (50-1), (60-2)] + [(10-20), (20-30),
	// (30-40), (40-50), (50-60), (10-50)] + 0, S

	private static SPTerm termBuilder(String list) {

		return null;
	}

	private static void testMinim(String argv[]) {
		try {
			System.out.println("testing minim");
			SICStus sp = new SICStus(argv, null);

			sp.load("dummy.pl");

			SPPredicate pred = new SPPredicate(sp, "dummy", 2, "");

			//constructing 20-1
			SPTerm[] t1 = new SPTerm[2];
			t1[0] = new SPTerm(sp, 20);
			t1[1] = new SPTerm(sp, 1);
			SPTerm term11 = new SPTerm(sp, "-", t1);
			
			//constructing 30-1
			SPTerm[] t2 = new SPTerm[2];
			t2[0] = new SPTerm(sp, 30);
			t2[1] = new SPTerm(sp, 1);
			SPTerm term12 = new SPTerm(sp, "-", t2);

			//constructing (20-1) + (30-1)
			SPTerm[] t3 = new SPTerm[2];
			t3[0] = term11;
			t3[1] = term12;
			SPTerm term21 = new SPTerm(sp, "+", t3);

			//alternative way for constructing (20-1) + (30-1)
			SPTerm[] t4 = new SPTerm[2];
			t4[0] = new SPTerm(sp, "20-1");
			t4[1] = new SPTerm(sp, "30-1");
			SPTerm term22 = new SPTerm(sp, "+", t4);
			
			//variable
			SPTerm size = new SPTerm(sp).putVariable();

			System.out.println("test construction #1");
			SPQuery query1 = sp.openQuery(pred, new SPTerm[] { term21, size });

			while (query1.nextSolution()) {
				System.out.println(size.toString());
			}
			
			System.out.println("test construction #2");
			SPQuery query2 = sp.openQuery(pred, new SPTerm[] { term22, size });

			while (query2.nextSolution()) {
				System.out.println(size.toString());
			}
			

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}