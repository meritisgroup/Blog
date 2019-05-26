package algo

import scala.annotation.tailrec

trait AlphaBeta[Node] {

	def eval(node: Node): Double

	def children(node: Node, maximize: Boolean): Seq[Node]

	def findBestNode(initialNode: Node, depth: Int): (Node, Double) = findBestNodeAlphaBeta(initialNode, depth, Double.MinValue, Double.MaxValue, true)

	def findBestNodeAlphaBeta(node: Node, depth: Int, alpha: Double, beta: Double, maximize: Boolean): (Node, Double) = {

		@tailrec
		def recMax(best: (Node, Double), childrenSeq: Seq[Node], alpha: Double, beta: Double): (Node, Double) = {
			if (childrenSeq.isEmpty) {
				best
			} else {
				val child = childrenSeq.head
				val childRes = findBestNodeAlphaBeta(child, depth - 1, alpha, beta, !maximize)
				val maxRes = if (childRes._2 > best._2) childRes else best
				if (maxRes._2 >= beta) maxRes
				else {
					val newAlpha = if (maxRes._2 > alpha) maxRes._2 else alpha
					recMax(maxRes, childrenSeq.tail, newAlpha, beta)
				}
			}
		}

		@tailrec
		def recMin(best: (Node, Double), childrenSeq: Seq[Node], alpha: Double, beta: Double): (Node, Double) = {
			if (childrenSeq.isEmpty) {
				best
			} else {
				val child = childrenSeq.head
				val childRes = findBestNodeAlphaBeta(child, depth - 1, alpha, beta, !maximize)
				val minRes = if (childRes._2 < best._2) childRes else best
				if (minRes._2 <= alpha) minRes
				else {
					val newBeta = if (minRes._2 < beta) minRes._2 else beta
					recMin(minRes, childrenSeq.tail, alpha, newBeta)
				}
			}
		}

		if (depth == 0) {
			// If we reached the initial depth, just evaluate the node
			(node, eval(node))

		} else {
			val nodeChildren = children(node, maximize)

			if (nodeChildren.isEmpty) {
				// If the node has no children, evaluation is accurate
				(node, eval(node))

			} else {
				val res = if (maximize) recMax((node, Double.MinValue), nodeChildren, alpha, beta)
							else recMin((node, Double.MaxValue), nodeChildren, alpha, beta)
				res
			}
		}		
	}

}
