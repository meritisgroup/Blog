package algo

import scala.annotation.tailrec
import TreeSearchAlgo._


object AlphaBeta {

	def findBestNode[Node](initial: Node, rules: GameRules[Node], hyper: HyperParameters): SearchResult[Node] = {

		val minSearchResult = SearchResult[Node](Double.MinValue, Nil)
		val maxSearchResult = SearchResult[Node](Double.MaxValue, Nil)

		def rec(node: Node, depth: Int, alpha: Double, beta: Double, maximize: Boolean): SearchResult[Node] = {
			
			@tailrec
			def recMax(best: SearchResult[Node], childrenSeq: List[Node], alpha: Double, beta: Double): SearchResult[Node] = {
				if (childrenSeq.isEmpty) {
					best
				} else {
					val childNode = childrenSeq.head
					val childResult = rec(childNode, depth - 1, alpha, beta, !maximize)

					val newBest = if (childResult.value > best.value) SearchResult(childResult.value, childNode :: childResult.bestChilds) else best

					if (newBest.value >= beta) {
						newBest
					} else {
						val newAlpha = if (newBest.value > alpha) newBest.value else alpha
						recMax(newBest, childrenSeq.tail, newAlpha, beta)
					}
				}
			}

			@tailrec
			def recMin(best: SearchResult[Node], childrenSeq: List[Node], alpha: Double, beta: Double): SearchResult[Node] = {
				if (childrenSeq.isEmpty) {
					best
				} else {
					val childNode = childrenSeq.head
					val childResult = rec(childNode, depth - 1, alpha, beta, !maximize)

					val newBest = if (childResult.value < best.value) SearchResult(childResult.value, childNode :: childResult.bestChilds) else best
					
					if (newBest.value <= alpha) {
						newBest
					} else {
						val newBeta = if (newBest.value < beta) newBest.value else beta
						recMin(newBest, childrenSeq.tail, alpha, newBeta)
					}
				}
			}

			if (depth == 0) {
				// If we reached the initial depth, just evaluate the node
				SearchResult(rules.evaluate(node, maximize), Nil)

			} else {
				val nodeChildren = rules.getChildren(node, maximize)

				if (nodeChildren.isEmpty) {
					// If the node has no children, evaluation is accurate
					SearchResult(rules.evaluate(node, maximize), Nil)

				} else if (maximize) {
					recMax(minSearchResult, nodeChildren, alpha, beta)
				} else {
					recMin(maxSearchResult, nodeChildren, alpha, beta)
				}
			}
		}

		rec(initial, hyper.maxDepth, Double.MinValue, Double.MaxValue, true)
	}

}
