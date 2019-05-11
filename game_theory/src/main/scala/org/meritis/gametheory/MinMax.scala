package org.meritis.gametheory

import scala.util.Random

// N is the type of a node
trait MinMax[Node] {

	def eval(node: Node): Double

	def children(node: Node): Seq[Node]

	def findBestNode(initial: Node, depth: Int, maximize: Boolean = true): (Node, Double) = {
		if (depth == 0) {
			// If we reached the initial depth, just evaluate the node
			(initial, eval(initial))

		} else {
			val nodeChildren = children(initial)

			if (nodeChildren.isEmpty) {
				// If the node has no children, evaluation is accurate
				(initial, eval(initial))

			} else {
				// Find related score for all children
				val childrenWithEval = for (child <- nodeChildren)
										yield (child, findBestNode(child, depth - 1, !maximize)._2)

				// Find the best score of all children
				val bestEval = if (maximize) childrenWithEval.maxBy(_._2)
								else childrenWithEval.minBy(_._2)

				// Look for all children with the best score
				val bestChildren = childrenWithEval.filter(_._2 == bestEval._2)

				// Pick up one randomly
				bestChildren(Random.nextInt(bestChildren.size))
			}
		}
	}

}
