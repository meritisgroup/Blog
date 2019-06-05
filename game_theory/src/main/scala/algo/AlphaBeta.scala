package algo

import scala.annotation.tailrec
import MinMax._


object AlphaBeta {

	def findBestNode[Node](initialNode: Node, maxDepth: Int, eval: EvalutionFct[Node], children: ChildrenFct[Node]): Result[Node] = {

		def intern(node: Node, depth: Int, alpha: Double, beta: Double, maximize: Boolean): Result[Node] = {
			@tailrec
			def recMax(best: Result[Node], childrenSeq: Seq[Node], alpha: Double, beta: Double): Result[Node] = {
				if (childrenSeq.isEmpty) {
					best
				} else {
					val childNode = childrenSeq.head
					val childResult = intern(childNode, depth - 1, alpha, beta, !maximize)
					val newBest = if (childResult.value > best.value) Result(childResult.value, childNode :: childResult.bestChilds) else best
					if (newBest.value >= beta) newBest
					else {
						val newAlpha = if (newBest.value > alpha) newBest.value else alpha
						recMax(newBest, childrenSeq.tail, newAlpha, beta)
					}
				}
			}

			@tailrec
			def recMin(best: Result[Node], childrenSeq: Seq[Node], alpha: Double, beta: Double): Result[Node] = {
				if (childrenSeq.isEmpty) {
					best
				} else {
					val childNode = childrenSeq.head
					val childResult = intern(childNode, depth - 1, alpha, beta, !maximize)
					val newBest = if (childResult.value < best.value) Result(childResult.value, childNode :: childResult.bestChilds) else best
					if (newBest.value <= alpha) newBest
					else {
						val newBeta = if (newBest.value < beta) newBest.value else beta
						recMin(newBest, childrenSeq.tail, alpha, newBeta)
					}
				}
			}

			if (depth == 0) {
				// If we reached the initial depth, just evaluate the node
				Result(eval(node, maximize), Nil)

			} else {
				val nodeChildren = children(node, maximize)

				if (nodeChildren.isEmpty) {
					// If the node has no children, evaluation is accurate
					Result(eval(node, maximize), Nil)

				} else if (maximize) {
					recMax(Result(Double.MinValue, Nil), nodeChildren, alpha, beta)
				} else {
					recMin(Result(Double.MaxValue, Nil), nodeChildren, alpha, beta)
				}
			}
		}

		intern(initialNode, maxDepth, Double.MinValue, Double.MaxValue, true)
	}

}
