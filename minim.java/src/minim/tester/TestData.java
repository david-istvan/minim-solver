package minim.tester;

import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.Maps;

public final class TestData {
	public static Map<Integer, Integer> getWeights() {
		HashMap<Integer, Integer> weights = Maps.newHashMap();
		// weights.put(1, 2);
		// weights.put(3, 4);
		weights.put(20, 1);
		weights.put(30, 1);
		weights.put(40, 2);
		weights.put(10, 1);
		weights.put(50, 1);
		weights.put(60, 2);
		return weights;
	}

	public static Map<Integer, Integer> getEdges() {
		HashMap<Integer, Integer> edges = Maps.newHashMap();
		// edges.put(5, 6);
		// edges.put(7, 8);
		edges.put(10, 20);
		edges.put(20, 30);
		edges.put(30, 40);
		edges.put(40, 50);
		edges.put(50, 60);
		edges.put(10, 50);
		return edges;
	}
}
