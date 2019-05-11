package org.meritis.gametheory

import org.scalatest.FunSuite

class MinMaxTest extends FunSuite {

	case class Node(eval: Double, children: List[Node] = Nil)

	class MinMaxAlgoTest extends MinMax[Node] {
		def eval(node: Node): Double = node.eval
		def children(node: Node): List[Node] = node.children
	}

	test("minimax with 1 level of children") {
		// Algo should pick up the highest one
		val initial = Node(0, List(Node(1), Node(2), Node(3)))
		val best = new MinMaxAlgoTest().findBestNode(initial, 1)
		assert(best === (Node(3), 3))
	}

	test("minimax with 2 levels of children") {
		val child1 = Node(0.5, List(Node(1), Node(2), Node(3)))
		val child2 = Node(-0.5, List(Node(1.1), Node(2.6)))
		val initial = Node(0, List(child1, child2))

		// Algo should pick up the node at depth 1 with the child assigned to the lowest value (ie 1.1 vs 1 => child2)
		val best = new MinMaxAlgoTest().findBestNode(initial, 2)
		assert(best === (child2, 1.1))

		// Try with only 1 level of depth to check if fathers are correctly evaluated
		val bestWith1Depth = new MinMaxAlgoTest().findBestNode(initial, 1)
		assert(bestWith1Depth === (child1, 0.5))
	}

	test("3rd level") {
		// Make sure that on the 3rd level, max is applied (and not min)
		val child1 = Node(0, List(Node(4.1), Node(5)))
		val father1 = Node(1, List(child1))
		val child2 = Node(2, List(Node(4), Node(6)))
		val father2 = Node(3, List(child2))
		val initial = Node(4, List(father1, father2))

		val best = new MinMaxAlgoTest().findBestNode(initial, 3)
		assert(best === (father2, 6))
	}

	test("pick up random node") {
		val child1 = Node(0, List(Node(1), Node(2), Node(3)))
		val child2 = Node(0, List(Node(1), Node(4)))
		val child3 = Node(0, List(Node(0.5), Node(0.6)))
		val initial = Node(0, List(child1, child2, child3))

		// As child1 and child2 have the same evaluation, they should be returned by the algo randomly
		val set = (for (n <- 0 until 1000) yield new MinMaxAlgoTest().findBestNode(initial, 5)._1).toSet
		assert(set contains child1)
		assert(set contains child2)
	}

}
