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
import com.google.common.collect.Maps;

public class Minim {
	private static final String PROLOG_FILE = "minim.pl";
	private static final String PREDICATE = "findMinimalSolution";
	private static final String FUNCTOR_LIST = ".";
	private static final String FUNCTOR_GRAPH = "+";
	private static final String FUNCTOR_PAIR = "-";
	private SICStus sp;

	public static class Graph {
		private Map<Integer, Integer> weights = Maps.newHashMap();
		private Map<Integer, Integer> edges = Maps.newHashMap();

		public Graph withWeights(Map<Integer, Integer> weights) {
			this.weights = weights;
			return this;
		}

		public Graph withEdges(Map<Integer, Integer> edges) {
			this.edges = edges;
			return this;
		}

		public int getDefaultSize() {
			return weights.size();
		}

		public Map<Integer, Integer> getWeights() {
			return weights;
		}

		public Map<Integer, Integer> getEdges() {
			return edges;
		}
	}

	public Minim() {
		try {
			sp = new SICStus();
			sp.load(PROLOG_FILE);
		} catch (SPException e) {
			e.printStackTrace();
		}
	}

	public Graph newGraph() {
		return new Graph();
	}

	public int findMinimalSolution(Graph graph) throws SPException {
		SPTerm weightList = constructKeyValueList(graph.getWeights());
		SPTerm edgeList = constructKeyValueList(graph.getEdges());
		SPTerm graphTerm = constructGraphStruct(weightList, edgeList);

		SPTerm size = new SPTerm(sp).putVariable();
		SPQuery query = sp.openQuery(new SPPredicate(sp, PREDICATE, 2, ""),
				new SPTerm[] { graphTerm, size });

		while (query.nextSolution()) {
			return Integer.parseInt(size.toString());
		}
		return graph.getDefaultSize();
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
		SPTerm pairTerm = new SPTerm(sp, FUNCTOR_PAIR, tmp);
		return pairTerm;
	}

	private SPTerm constructGraphStruct(SPTerm wl, SPTerm el) throws ConversionFailedException, IllegalTermException {
		List<SPTerm> tmp = Lists.newArrayList();
		tmp.add(wl);
		tmp.add(el);
		SPTerm weightAndEdgesTerm = new SPTerm(sp, FUNCTOR_GRAPH, tmp.toArray(new SPTerm[tmp.size()]));

		tmp = Lists.newArrayList();
		tmp.add(weightAndEdgesTerm);
		tmp.add(new SPTerm(sp, 0));

		return new SPTerm(sp, FUNCTOR_GRAPH, tmp.toArray(new SPTerm[tmp.size()]));
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
		SPTerm newListTerm = new SPTerm(sp, FUNCTOR_LIST, tmp);
		return newListTerm;
	}

	private SPTerm getLastListTerm(SICStus sp, SPTerm term) throws ConversionFailedException, IllegalTermException {
		SPTerm emptyList = new SPTerm(sp).putEmptyList();
		SPTerm[] tmp = new SPTerm[2];
		tmp[0] = term;
		tmp[1] = emptyList;
		SPTerm lastListTerm = new SPTerm(sp, FUNCTOR_LIST, tmp);
		return lastListTerm;
	}
}