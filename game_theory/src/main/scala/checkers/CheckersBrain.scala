package checkers

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import collection.JavaConverters._
import algo.HyperParameters
import algo.TreeSearchAlgo._
import algo.GameRules
import algo.AlphaBeta


class CheckersRules(log: Boolean) extends GameRules[Moves] {

	val count = new AtomicInteger(0)
	val evaluationsSet = scala.collection.mutable.SortedSet[Double]()
	val childrenCountAcc = new AtomicInteger(0)

	val cache = new java.util.concurrent.ConcurrentHashMap[Long, Moves]().asScala

	def createNode(board: Board, sideToPlay: Color): Moves = {
		val hash = Moves.computeHash(board, sideToPlay)

		if (cache contains hash) {
			cache(hash)
		} else {
			val node = new Moves(board, sideToPlay)
			cache += (node.hash -> node)

			if (log) {
				count.incrementAndGet
				childrenCountAcc.addAndGet(node.legalMoves.size)
			}

			node
		}
	}

	override def evaluate(node: Moves, maximize: Boolean): Double = {
		val result = Evaluation.evaluate(node)
		if (log) {
			evaluationsSet += result
		}
		result
	}

	override def getChildren(node: Moves, maximize: Boolean): List[Moves] = {
		if (node.win == Won || node.win == Lost) {
			Nil
		} else {
			val otherSide = !node.sideToPlay
			val moves = node.legalMoves

			moves map { move => createNode(move.after, otherSide) }
		}
	}

}


class CheckersBrain(val side: Color,
					searchFct: BestNodeFct[Moves] = AlphaBeta.findBestNode[Moves],
					hyper: HyperParameters = HyperParameters(10),
					log: Boolean = false) {

	val rules = new CheckersRules(log)

	def expandCount: Option[Int] = {
		if (!log) None
		else Some(rules.count.get)
	}

	def evaluationsSet: Option[Set[Double]] = {
		if (!log) None
		else Some(rules.evaluationsSet.toSet)
	}

	def branchingFactor: Option[Double] = {
		if (!log) None
		else Some(rules.childrenCountAcc.get.toDouble / rules.count.get)
	}

	def bestMove(board: Board): (Option[Move], Double) = {
		val initialNode = new Moves(board, side)

		if (initialNode.legalMoves.size == 1) {
			val uniqueMove = initialNode.legalMoves.head
			(Some(uniqueMove), rules.evaluate(initialNode, true))

		} else {
			val result = searchFct(initialNode, rules, hyper)

			val bestNode = if (result.bestChilds.isEmpty) None else Some(result.bestChilds.head)
			val move = bestNode flatMap { node => initialNode.legalMoves.find(move => move.after == node.current) }

			(move, result.value)
		}
	}

}
