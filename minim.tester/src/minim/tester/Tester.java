package minim.tester;

import minim.tester.Minim.Graph;
import se.sics.jasper.SPException;

public class Tester {

	public static void main(String[] args) {
		try {
			Minim minim = new Minim();
			Graph graph = minim.newGraph().withWeights(TestData.getWeights()).withEdges(TestData.getEdges());
			int minimalSolution = minim.findMinimalSolution(graph);
			System.out.println("Minimal solution found: " + minimalSolution + ".");
		} catch (SPException e) {
			e.printStackTrace();
		}
	}
}
