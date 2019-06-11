package algo


abstract class GameRules[Node] {

	// Return an estimated value of the node (between -1.0 and +1.0)
	def evaluate(node: Node, maximize: Boolean): Double

	// Compute children of the input node
	// Order can be important depending on the algorithm
	def getChildren(node: Node, maximize: Boolean): List[Node]

}


case class SearchResult[Node](value: Double, bestChilds: List[Node])


case class HyperParameters(maxDepth: Int)


object TreeSearchAlgo {

	type BestNodeFct[Node] = (Node, GameRules[Node], HyperParameters) => SearchResult[Node]

}
