package org.meritis.gametheory

import org.scalatest.FunSuite

class AlphaBetaTest extends MinMaxTest {

	class AlphaBetaAlgoTest extends AlphaBeta[Node] {
		def eval(node: Node): Double = node.eval
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

}
