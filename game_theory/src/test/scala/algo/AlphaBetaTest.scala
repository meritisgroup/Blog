package algo

import org.scalatest.FunSuite

class AlphaBetaTest extends MinMaxTest {

	class AlphaBetaAlgoTest extends AlphaBeta[Node] {
		var evalCount: Int = 0
		def eval(node: Node): Double = {
			evalCount += 1
			node.eval
		}
		def children(node: Node, maximize: Boolean): List[Node] = node.children
	}

	test("same results as min max") {
		new Level1 {
			val bestMinMax = new MinMaxAlgoTest().findBestNode(initial, 1)
			val bestAlphaBeta = new AlphaBetaAlgoTest().findBestNode(initial, 1)
			assert(bestMinMax === bestAlphaBeta)
		}
		new Level2 {
			val bestMinMax = new MinMaxAlgoTest().findBestNode(initial, 1)
			val bestAlphaBeta = new AlphaBetaAlgoTest().findBestNode(initial, 1)
			assert(bestMinMax === bestAlphaBeta)
		}
		new Level3 {
			val bestMinMax = new MinMaxAlgoTest().findBestNode(initial, 1)
			val bestAlphaBeta = new AlphaBetaAlgoTest().findBestNode(initial, 1)
			assert(bestMinMax === bestAlphaBeta)
		}
	}

	test("alpha cut") {
		// See following page to check example tree
		// https://fr.wikipedia.org/wiki/%C3%89lagage_alpha-b%C3%AAta#Principe for 
		val child1 = Node(5)
		val child2 = Node(0, List(Node(4), Node(5), Node(6), Node(7), Node(8)))
		val initial = Node(0, List(child1, child2))
		val algo = new AlphaBetaAlgoTest()
		algo.findBestNode(initial, 10)
		assert(algo.evalCount === 2)
	}

	test("beta cut") {
		// See following page to check example tree
		// https://fr.wikipedia.org/wiki/%C3%89lagage_alpha-b%C3%AAta#Principe for 
		val child1 = Node(3)
		val child2 = Node(0, List(Node(4), Node(5), Node(6), Node(7), Node(8)))
		val initial = Node(0, List(child1, child2))
		val algo = new AlphaBetaAlgoTest()
		algo.findBestNodeAlphaBeta(initial, 10, Double.MinValue, Double.MaxValue, false)
		assert(algo.evalCount === 2)
	}

}
