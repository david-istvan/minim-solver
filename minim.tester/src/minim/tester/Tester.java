package minim.tester;

import se.sics.jasper.SPException;

public class Tester {

	public static void main(String[] args) {
		try {
			MinimAPI minimAPI = new MinimAPI(args);
			minimAPI.setGraphData(TestData.getWeights(), TestData.getEdges());
			minimAPI.execute();
		} catch (SPException e) {
			e.printStackTrace();
		}
	}
}
