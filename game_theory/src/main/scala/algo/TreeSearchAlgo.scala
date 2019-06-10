package algo


abstract class GameRules[Node] {

	// Return an estimated value of the node (between -1.0 and +1.0)
	def evaluate(node: Node, maximize: Boolean): Double

	// Compute children of the input node
	// Order can be important depending on the algorithm
	def getChildren(node: Node, maximize: Boolean): List[Node]

	// Return the probability of each move
	// The sum should be equal to 1
	def expertPolicy(parent: Node, children: List[Node]): List[Double] = {
		val probability = 1.0 / children.size
		List.fill(children.size)(probability)
	}

}


case class SearchResult[Node](value: Double, bestChilds: List[Node])


case class HyperParameters(maxDepth: Int)


object TreeSearchAlgo {

	type BestNodeFct[Node] = (Node, GameRules[Node], HyperParameters) => SearchResult[Node]

}
