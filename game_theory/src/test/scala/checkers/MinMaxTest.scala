package checkers

import org.scalatest.FunSuite


class MinMaxTest extends FunSuite {

	case class Node(eval: Double, children: List[Node] = Nil)

	def eval(node: Node): Double = node.eval
	def children(node: Node, maximize: Boolean): List[Node] = node.children

	trait Level1 {
		val initial = Node(0, List(Node(1), Node(2), Node(3)))
	}

	trait Level2 {
		val child1 = Node(0.5, List(Node(1), Node(2), Node(3)))
		val child2 = Node(-0.5, List(Node(1.1), Node(2.6)))
		val initial = Node(0, List(child1, child2))
	}

	trait Level3 {
		val child1 = Node(0, List(Node(4.1), Node(5)))
		val father1 = Node(1, List(child1))
		val child2 = Node(2, List(Node(4), Node(6)))
		val father2 = Node(3, List(child2))
		val initial = Node(4, List(father1, father2))
	}

	trait Level4 {
		val child1 = Node(0, List(Node(1), Node(2), Node(3)))
		val child2 = Node(0, List(Node(1), Node(4)))
		val child3 = Node(0, List(Node(0.5), Node(0.6)))
		val initial = Node(0, List(child1, child2, child3))
	}

	test("1 level of children") {
		// Algo should pick up the highest one
		new Level1 {
			val best = new MinMax[Node]().findBestNode(initial, 1, eval, children)
			assert(best === (Node(3), 3))
		}
	}

	test("2 levels of children") {
		new Level2 {
			// Algo should pick up the node at depth 1 with the child assigned to the lowest value (ie 1.1 vs 1 => child2)
			val best = new MinMax[Node]().findBestNode(initial, 2, eval, children)
			assert(best === (child2, 1.1))

			// Try with only 1 level of depth to check if fathers are correctly evaluated
			val bestWith1Depth = new MinMax[Node]().findBestNode(initial, 1, eval, children)
			assert(bestWith1Depth === (child1, 0.5))
		}
	}

	test("3rd level") {
		new Level3 {
			// Make sure that on the 3rd level, max is applied (and not min)
			val best = new MinMax[Node]().findBestNode(initial, 3, eval, children)
			assert(best === (father2, 6))
		}
	}

	test("pick up random node") {
		new Level4 {
			// As child1 and child2 have the same evaluation, they should be returned by the algo randomly
			val set = (for (n <- 0 until 1000) yield new MinMax[Node]().findBestNode(initial, 5, eval, children)._1).toSet
			assert(set contains child1)
			assert(set contains child2)
		}
	}

}
