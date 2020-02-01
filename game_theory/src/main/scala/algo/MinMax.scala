package algo

import scala.util.Random
import TreeSearchAlgo._


object MinMax {

	def findBestNode[Node](initial: Node, rules: GameRules[Node], hyper: HyperParameters): SearchResult[Node] = {

		def rec(node: Node, depth: Int, maximize: Boolean): SearchResult[Node] = {
			if (depth == 0) {
				// If we reached the initial depth, just evaluate the node
				SearchResult(rules.evaluate(node, maximize), Nil)

			} else {
				val nodeChildren = rules.getChildren(node, maximize)

				if (nodeChildren.isEmpty) {
					// If the node has no children, evaluation is accurate
					SearchResult(rules.evaluate(node, maximize), Nil)

				} else {
					// Find related score for all children
					val childrenWithEval = for (child <- nodeChildren)
											yield (child, rec(child, depth - 1, !maximize))

					// Find the best score of all children
					val bestEval = if (maximize) childrenWithEval.maxBy(_._2.value)
									else childrenWithEval.minBy(_._2.value)
					val bestValue = bestEval._2.value

					// Look for all children with the best score
					val bestChildren = childrenWithEval.filter(_._2.value == bestValue)

					// Pick up one randomly
					val bestChild = if (bestChildren.size == 1) bestChildren.head
									else bestChildren(Random.nextInt(bestChildren.size))
						
					SearchResult(bestValue, bestChild._1 :: bestChild._2.bestChilds)
				}
			}
		}

		rec(initial, hyper.maxDepth, true)
	}

}
