package checkers

import scala.util.Random


class MinMax[Node] {

	type EvalutionFct = Node => Double

	type ChildrenFct = (Node, Boolean) => Seq[Node]

	def findBestNode(initialNode: Node, maxDepth: Int, eval: EvalutionFct, children: ChildrenFct): (Node, Double) = {

		def rec(node: Node, depth: Int, maximize: Boolean): (Node, Double) = {
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
											yield (child, rec(child, depth - 1, !maximize)._2)

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

		rec(initialNode, maxDepth, true)
	}

}
