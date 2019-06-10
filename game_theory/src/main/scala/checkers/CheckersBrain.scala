package checkers

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import algo.HyperParameters
import algo.TreeSearchAlgo._
import algo.GameRules
import algo.MinMax
import algo.AlphaBeta


case class CheckersNode(current: Board, lastMove: Option[Move], nextMoves: Moves)


class CheckersRules(log: Boolean) extends GameRules[CheckersNode] {

	val count = new AtomicInteger(0)
	val evaluationsSet = scala.collection.mutable.SortedSet[Double]()

	override def evaluate(node: CheckersNode, maximize: Boolean): Double = {
		val result = Evaluation.evaluate(node.nextMoves)
		if (log) {
			count.incrementAndGet
			evaluationsSet += result
		}
		result
	}

	override def getChildren(node: CheckersNode, maximize: Boolean): List[CheckersNode] = {
		if (log) {
			count.incrementAndGet
		}

		if (node.nextMoves.win == Won || node.nextMoves.win == Lost) {
			Nil
		} else {
			val otherSide = !node.nextMoves.sideToPlay
			val moves = node.nextMoves.legalMoves
			val nodes = moves map { move => CheckersNode(move.after, Some(move), new Moves(move.after, otherSide)) }

			if (maximize) nodes.sortBy { node => - Evaluation.evaluate(node.nextMoves) }
			else nodes.sortBy { node => Evaluation.evaluate(node.nextMoves) }
		}
	}

}


class CheckersBrain(side: Color,
					searchFct: BestNodeFct[CheckersNode] = AlphaBeta.findBestNode[CheckersNode],
					hyper: HyperParameters = HyperParameters(10),
					log: Boolean = true) {

	val rules = new CheckersRules(log)

	def expandCount: Option[Int] = {
		if (!log) None
		else Some(rules.count.get)
	}

	def evaluationsSet: Option[Set[Double]] = {
		if (!log) None
		else Some(rules.evaluationsSet.toSet)
	}

	def bestMove(board: Board): (Option[Move], Double) = {
		val initialNode = CheckersNode(board, None, new Moves(board, side))

		if (initialNode.nextMoves.legalMoves.size == 1) {
			val uniqueMove = initialNode.nextMoves.legalMoves.head
			(Some(uniqueMove), rules.evaluate(initialNode, true))

		} else {
			val result = searchFct(initialNode, rules, hyper)

			val bestNode = if (result.bestChilds.isEmpty) None else result.bestChilds.head.lastMove

			(bestNode, result.value)
		}
	}

}
