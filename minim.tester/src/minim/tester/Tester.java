package minim.tester;

import se.sics.jasper.*;

public class Tester {
	public static void main(String argv[]) {
		try {
			SICStus sp = new SICStus(argv, null);

			sp.load("example.pl");

			SPPredicate pred = new SPPredicate(sp, "sum", 3, "");
			SPTerm aTrue = new SPTerm(sp, 10);
			SPTerm aFalse = new SPTerm(sp, 12);
			SPTerm x1 = new SPTerm(sp, 4);
			SPTerm x2 = new SPTerm(sp, 6);

			boolean query1 = sp.query(pred, new SPTerm[] { aTrue, x1, x2 });
			System.out.println(query1);
			
			boolean query2 = sp.query(pred, new SPTerm[] { aFalse, x1, x2 });
			System.out.println(query2);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}