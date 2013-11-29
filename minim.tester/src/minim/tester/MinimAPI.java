package minim.tester;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import se.sics.jasper.ConversionFailedException;
import se.sics.jasper.IllegalTermException;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPException;
import se.sics.jasper.SPPredicate;
import se.sics.jasper.SPQuery;
import se.sics.jasper.SPTerm;

import com.google.common.collect.Lists;

public class MinimAPI {
	private SICStus sp;
	private Map<Integer, Integer> weights;
	private Map<Integer, Integer> edges;

	public MinimAPI(String argv[]) throws SPException {
		sp = new SICStus(argv, null);
		sp.load("minim.pl");
	}

	public void setGraphData(Map<Integer, Integer> weights, Map<Integer, Integer> edges) {
		this.weights = weights;
		this.edges = edges;
	}

	public void execute() throws SPException {
		SPTerm weightList = constructKeyValueList(weights);
		SPTerm edgeList = constructKeyValueList(edges);
		SPTerm graph = constructGraphStruct(weightList, edgeList);

		SPTerm size = new SPTerm(sp).putVariable();
		System.out.println("test graph");
		SPQuery query = sp.openQuery(new SPPredicate(sp, "findMinimalSolution", 2, ""), new SPTerm[] { graph, size });

		while (query.nextSolution()) {
			System.out.println(size.toString());
		}
	}

	private SPTerm constructKeyValueList(Map<Integer, Integer> pairs) throws ConversionFailedException,
			IllegalTermException {
		List<SPTerm> pairList = Lists.newArrayList();

		for (Entry<Integer, Integer> entry : pairs.entrySet()) {
			pairList.add(createPairTerm(entry.getKey(), entry.getValue()));
		}

		SPTerm pairListTerm = buildListTerm(pairList);
		return pairListTerm;
	}

	private SPTerm createPairTerm(int key, int value) throws ConversionFailedException, IllegalTermException {
		SPTerm[] tmp = new SPTerm[2];
		tmp[0] = new SPTerm(sp, key);
		tmp[1] = new SPTerm(sp, value);
		SPTerm pairTerm = new SPTerm(sp, "-", tmp);
		return pairTerm;
	}

	private SPTerm constructGraphStruct(SPTerm wl, SPTerm el) throws ConversionFailedException, IllegalTermException {
		List<SPTerm> tmp = Lists.newArrayList();
		tmp.add(wl);
		tmp.add(el);
		SPTerm weightAndEdgesTerm = new SPTerm(sp, "+", tmp.toArray(new SPTerm[tmp.size()]));
		
		tmp = Lists.newArrayList();
		tmp.add(weightAndEdgesTerm);
		tmp.add(new SPTerm(sp, 0));

		return new SPTerm(sp, "+", tmp.toArray(new SPTerm[tmp.size()]));
	}

	public SPTerm buildListTerm(List<SPTerm> terms) throws ConversionFailedException, IllegalTermException {
		List<SPTerm> reversedTermList = Lists.reverse(terms);
		SPTerm listTerm = getLastListTerm(sp, reversedTermList.remove(0));

		for (SPTerm term : reversedTermList) {
			listTerm = addOtherTerms(sp, listTerm, term);
		}

		return listTerm;
	}

	private SPTerm addOtherTerms(SICStus sp, SPTerm listTerm, SPTerm nextTerm) throws ConversionFailedException,
			IllegalTermException {
		SPTerm[] tmp = new SPTerm[2];
		tmp[0] = nextTerm;
		tmp[1] = listTerm;
		SPTerm newListTerm = new SPTerm(sp, ".", tmp);
		return newListTerm;
	}

	private SPTerm getLastListTerm(SICStus sp, SPTerm term) throws ConversionFailedException, IllegalTermException {
		SPTerm emptyList = new SPTerm(sp).putEmptyList();
		SPTerm[] tmp = new SPTerm[2];
		tmp[0] = term;
		tmp[1] = emptyList;
		SPTerm lastListTerm = new SPTerm(sp, ".", tmp);
		return lastListTerm;
	}

	// private void test(SPTerm listTerm) throws SPException {
	// SPTerm size = new SPTerm(sp).putVariable();
	//
	// System.out.println("test list");
	// SPQuery query3 = sp.openQuery(new SPPredicate(sp, "size2", 2, ""), new
	// SPTerm[] { listTerm, size });
	//
	// while (query3.nextSolution()) {
	// System.out.println(size.toString());
	// }
	// }
	//
	// private List<SPTerm> getAtomicTerms() {
	// List<SPTerm> terms = Lists.newArrayList();
	// terms.add(new SPTerm(sp, 20));
	// terms.add(new SPTerm(sp, 30));
	// terms.add(new SPTerm(sp, 40));
	// return terms;
	// }
	//
	//
	//
	// private static void testMinim(String argv[]) {
	// try {
	// System.out.println("testing minim");
	// SICStus sp = new SICStus(argv, null);
	//
	// sp.load("dummy.pl");
	//
	// // constructing 20-1
	// SPTerm[] t1 = new SPTerm[2];
	// t1[0] = new SPTerm(sp, 20);
	// t1[1] = new SPTerm(sp, 1);
	// SPTerm term11 = new SPTerm(sp, "-", t1);
	//
	// // constructing 30-1
	// SPTerm[] t2 = new SPTerm[2];
	// t2[0] = new SPTerm(sp, 30);
	// t2[1] = new SPTerm(sp, 1);
	// SPTerm term12 = new SPTerm(sp, "-", t2);
	//
	// // constructing (20-1) + (30-1)
	// SPTerm[] t3 = new SPTerm[2];
	// t3[0] = term11;
	// t3[1] = term12;
	// SPTerm term21 = new SPTerm(sp, "+", t3);
	//
	// // alternative way for constructing (20-1) + (30-1)
	// SPTerm[] t4 = new SPTerm[2];
	// t4[0] = new SPTerm(sp, "20-1");
	// t4[1] = new SPTerm(sp, "30-1");
	// SPTerm term22 = new SPTerm(sp, "+", t4);
	//
	// // TODO
	// // constructing list
	// SPTerm[] listTermElements = new SPTerm[2];
	// listTermElements[0] = term11;
	// listTermElements[1] = term12;
	// SPTerm emptyList = new SPTerm(sp).putEmptyList();
	// SPTerm term20 = new SPTerm(sp, 20);
	// SPTerm[] tmp = new SPTerm[2];
	// tmp[0] = term20;
	// tmp[1] = emptyList;
	// SPTerm listTerm = new SPTerm(sp, ".", tmp);
	//
	// SPTerm term30 = new SPTerm(sp, 30);
	// tmp[0] = term30;
	// tmp[1] = listTerm;
	// listTerm = new SPTerm(sp, ".", tmp);
	//
	// // SPTerm listTerm = (SPTerm) sp.consList(new SPTerm(sp, 20), new
	// // SPTerm(sp, 1));
	// // SPTerm listTerm = new SPTerm(sp, ".", listTermElements);
	//
	// // variable
	// SPTerm size = new SPTerm(sp).putVariable();
	//
	// System.out.println("test construction #1");
	// SPQuery query1 = sp.openQuery(new SPPredicate(sp, "dummy", 2, ""), new
	// SPTerm[] { term21, size });
	//
	// while (query1.nextSolution()) {
	// System.out.println(size.toString());
	// }
	//
	// System.out.println("test construction #2");
	// SPQuery query2 = sp.openQuery(new SPPredicate(sp, "dummy", 2, ""), new
	// SPTerm[] { term22, size });
	//
	// while (query2.nextSolution()) {
	// System.out.println(size.toString());
	// }
	//
	// System.out.println("test list");
	// SPQuery query3 = sp.openQuery(new SPPredicate(sp, "size2", 2, ""), new
	// SPTerm[] { listTerm, size });
	//
	// while (query3.nextSolution()) {
	// System.out.println(size.toString());
	// }
	//
	// } catch (Exception e) {
	// e.printStackTrace();
	// }
	// }
}