package tictactoe

import algo.MinMax
import TicTacToe._

class MinMaxTicTacToeAdapter(side: Mark) extends MinMax[Grid] {

	override val initialDepth = 100

	val posInfinity = 1000.0
	val negInfinity = -posInfinity

	val opposite = if (side == Cross) Circle else Cross

	def eval(node: Grid): Double = {
		if (win(node, side)) posInfinity
		else if (win(node, opposite)) negInfinity
		else {
			val marks = node partition (m => m == side)
			marks._1.size - marks._2.size
		}
	}

	def children(node: Grid, maximize: Boolean): List[Grid] = {
		if (win(node, side) || win(node, opposite)) Nil
		else {
			val sideToPlay = if (maximize) side else opposite
			legalMoves(node, sideToPlay) map (_._1)
		}
	}

}
