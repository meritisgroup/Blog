package algo

import org.scalatest.FunSuite
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.PlayTicTacToe._


class MinMaxTTT(side: Mark) {

	def eval(grid: Grid, maximize: Boolean): Double = {
		if (win(grid, side)) 1
		else if (win(grid, opponent(side))) -1
		else 0
	}

	def children(grid: Grid, maximize: Boolean): List[Grid] = {
		val sideToPlay = if (maximize) side else opponent(side)
		if (win(grid, side) || win(grid, opponent(side))) Nil
		else legalMoves(grid, sideToPlay).map { elt => elt._1 }
	}

}


abstract class AlgoTest extends FunSuite {

	type Player = (Mark, Grid) => Grid

	def play(side: Mark, grid: Grid, player1: Player, player2: Player, list: List[Grid] = Nil): (Mark, List[Grid]) = {
		val grid1 = player1(side, grid)
		if (win(grid1, side)) {
			(side, grid1 :: list)
		} else {
			if (grid1.filter(_ == Empty).size == 0) {
				(Empty, grid1 :: list)
			} else {
				play(opponent(side), grid1, player2, player1, grid1 :: list)
			}
		}
	}

	def playerMinMax(side: Mark, grid: Grid): Grid = {
		val adapter = new MinMaxTTT(side)
		MinMax.findBestNode[Grid](grid, 20, adapter.eval, adapter.children).bestChilds.head
	}

	def playerAlphaBeta(side: Mark, grid: Grid): Grid = {
		val adapter = new MinMaxTTT(side)
		AlphaBeta.findBestNode[Grid](grid, 20, adapter.eval, adapter.children).bestChilds.head
	}

}
