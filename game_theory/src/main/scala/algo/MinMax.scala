package algo

import scala.util.Random


object MinMax {

	type EvalutionFct[Node] = (Node, Boolean) => Double

	type ChildrenFct[Node] = (Node, Boolean) => Seq[Node]

	case class Result[Node](value: Double, bestChilds: List[Node])

	def findBestNode[Node](initialNode: Node, maxDepth: Int, eval: EvalutionFct[Node], children: ChildrenFct[Node]): Result[Node] = {

		def rec(node: Node, depth: Int, maximize: Boolean): Result[Node] = {
			if (depth == 0) {
				// If we reached the initial depth, just evaluate the node
				Result(eval(node, maximize), Nil)

			} else {
				val nodeChildren = children(node, maximize)

				if (nodeChildren.isEmpty) {
					// If the node has no children, evaluation is accurate
					Result(eval(node, maximize), Nil)

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
						
					Result(bestValue, bestChild._1 :: bestChild._2.bestChilds)
				}
			}
		}

		rec(initialNode, maxDepth, true)
	}

}
