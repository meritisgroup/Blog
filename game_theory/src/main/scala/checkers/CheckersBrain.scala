package checkers

import java.util.concurrent.atomic.AtomicInteger
import algo.MinMax
import algo.MinMax._


case class CheckersNode(board: Board, move: Option[Move])


class EvaluationWrapper(eval: EvalutionFct[CheckersNode]) {

	val count = new AtomicInteger(0)
	val set = scala.collection.mutable.SortedSet[Double]()

	def evaluateAndLog(node: CheckersNode, maximize: Boolean): Double = {
		val result = eval(node, maximize)
		count.incrementAndGet
		set += result
		result
	}

}


class CheckersBrain(side: Color, log: Boolean) {

	def directEval(node: CheckersNode, maximize: Boolean): Double = Evaluation.evaluate(node.board, side)

	val evalWrapper = new EvaluationWrapper(directEval)

	def evaluationsCount: Option[Int] = {
		if (!log) None
		else Some(evalWrapper.count.get)
	}

	def evaluationsGetAll: Option[Set[Double]] = {
		if (!log) None
		else Some(evalWrapper.set.toSet)
	}

	def getChildren(node: CheckersNode, maximize: Boolean): Seq[CheckersNode] = {
		val sideToPlay = if (maximize) side else !side
		if (Move.win(node.board, sideToPlay) == Won) {
			Nil
		} else {
			val moves = Move.legalMoves(node.board, sideToPlay)
			moves map { move => CheckersNode(move.after, Some(move)) }
		}
	}

	def bestMove(board: Board): (Option[Move], Double) = {
		val initialNode = CheckersNode(board, None)
		val maxDepth = 6
		val eval: EvalutionFct[CheckersNode] = if (log) evalWrapper.evaluateAndLog else directEval

		val result = MinMax.findBestNode[CheckersNode](initialNode, maxDepth, eval, getChildren)

		val bestNode = if (result.bestChilds.isEmpty) None else result.bestChilds.head.move

		(bestNode, result.value)
	}

}
