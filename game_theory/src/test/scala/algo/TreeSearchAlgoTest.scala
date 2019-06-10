package algo

import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.FunSuite
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.PlayTicTacToe._
import TreeSearchAlgo._


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

	type Player = (Mark, Grid) => Grid

	def minMaxPlayer(side: Mark, grid: Grid): Grid = {
		val rules = new TicTacToeRules(side)
		val searchResult = MinMax.findBestNode[Grid](grid, rules, hyper)
		searchResult.bestChilds.head
	}

	def alphaBetaPlayer(side: Mark, grid: Grid): Grid = {
		val rules = new TicTacToeRules(side)
		val searchResult = AlphaBeta.findBestNode[Grid](grid, rules, hyper)
		searchResult.bestChilds.head
	}

	def play(side: Mark, grid: Grid, player1: Player, player2: Player, list: List[Grid] = Nil): (Mark, List[Grid]) = {
		val nextGrid = player1(side, grid)

		if (win(nextGrid, side)) {
			(side, nextGrid :: list)
		} else if (nextGrid.filter(_ == Empty).size == 0) {
			(Empty, nextGrid :: list)
		} else {
			play(opponent(side), nextGrid, player2, player1, nextGrid :: list)
		}
	}
}


abstract class TreeSearchAlgoTest extends FunSuite {

}
