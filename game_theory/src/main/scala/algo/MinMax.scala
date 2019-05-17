package algo

import scala.util.Random

trait MinMax[Node] {

	def eval(node: Node): Double

	def children(node: Node, maximize: Boolean): Seq[Node]

	def initialDepth: Int = 10

	def findBestNode(initialNode: Node): (Node, Double) = findBestNodeMinMax(initialNode, initialDepth, true)

	def findBestNode(initialNode: Node, depth: Int): (Node, Double) = findBestNodeMinMax(initialNode, depth, true)

	def findBestNodeMinMax(node: Node, depth: Int, maximize: Boolean): (Node, Double) = {
		if (depth == 0) {
			// If we reached the initial depth, just evaluate the node
			(node, eval(node))

		} else {
			val nodeChildren = children(node, maximize)

			if (nodeChildren.isEmpty) {
				// If the node has no children, evaluation is accurate
				(node, eval(node))

			} else {
				// Find related score for all children
				val childrenWithEval = for (child <- nodeChildren)
										yield (child, findBestNodeMinMax(child, depth - 1, !maximize)._2)

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
