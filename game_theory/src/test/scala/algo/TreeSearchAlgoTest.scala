package algo

import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.FunSuite
import scala.util.Random
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.PlayTicTacToe._
import TreeSearchAlgo._
import Tournament._


case class Node(eval: Double, children: List[Node] = Nil)


class NodeRules extends GameRules[Node] {

	val counter = new AtomicInteger(0)

	override def evaluate(node: Node, maximize: Boolean): Double = {
		counter.incrementAndGet
		node.eval
	}

	override def getChildren(node: Node, maximize: Boolean): List[Node] = {
		node.children
	}
}


class TicTacToeRules(side: Mark) extends GameRules[Grid] {

	override def evaluate(grid: Grid, maximize: Boolean): Double = {
		if (win(grid, side)) 1.0
		else if (win(grid, opponent(side))) -1.0
		else 0
	}

	override def getChildren(grid: Grid, maximize: Boolean): List[Grid] = {
		val sideToPlay = if (maximize) side else opponent(side)
		if (win(grid, side) || win(grid, opponent(side))) Nil
		else legalMoves(grid, sideToPlay).map(_._1)
	}

}


object TicTacToeAutoPlayer {

	val hyper = HyperParameters(20)

	def randomPlayer(grid: Grid, side: Mark): Grid = {
		val moves = legalMoves(grid, side).map(_._1)
		moves(Random.nextInt(moves.size))
	}

	def minMaxPlayer(grid: Grid, side: Mark): Grid = {
		val rules = new TicTacToeRules(side)
		val searchResult = MinMax.findBestNode[Grid](grid, rules, hyper)
		searchResult.bestChilds.head
	}

	def alphaBetaPlayer(grid: Grid, side: Mark): Grid = {
		val rules = new TicTacToeRules(side)
		val searchResult = AlphaBeta.findBestNode[Grid](grid, rules, hyper)
		searchResult.bestChilds.head
	}

	def play(side: Mark, grid: Grid, player1: Player[Grid, Mark], player2: Player[Grid, Mark]): (Option[Mark], List[Grid]) = {
		Tournament.play(grid, side, opponent(side), player1, player2, TicTacToe.win, TicTacToe.draw)
	}
}
