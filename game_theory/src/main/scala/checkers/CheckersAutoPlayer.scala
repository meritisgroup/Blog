package checkers

import algo.Tournament
import algo.Tournament.Player


object CheckersAutoPlayer {

	def brainToPlayer(brain: CheckersBrain): Player[Board, Color] = {
		def fct(board: Board, side: Color): Board = {
			brain.bestMove(board, side)._1.get.after
		}

		fct
	}

	def win(board: Board, side: Color): Boolean = {
		new Moves(board, !side).win == Lost
	}

	def draw(board: Board, side: Color): Boolean = {
		false
	}

	def play(side: Color, board: Board, player1: CheckersBrain, player2: CheckersBrain): (Option[Color], List[Board]) = {
		Tournament.play(board, side, !side, brainToPlayer(player1), brainToPlayer(player2), win, draw)
	}

}
