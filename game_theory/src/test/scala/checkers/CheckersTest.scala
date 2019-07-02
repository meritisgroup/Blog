package checkers

import org.scalatest.FunSuite
import algo.Tournament


abstract class CheckersTest extends FunSuite {

	test("data referential") {
		assert(White.white)
		assert(Black.black)

		Pos.all.foreach { pos =>
			assert(pos != null)
			assert(pos.downLeft != null)
			assert(pos.downRight != null)
			assert(pos.upLeft != null)
			assert(pos.upRight != null)
		}

		Direction.all.foreach { dir =>
			assert(dir != null)
			assert(dir.perpendiculars != null)
			dir.perpendiculars.foreach { d => assert(d != null) }
		}

		assert(Piece(White, Queen).hashCode != Piece(Black, Pawn).hashCode)
	}

}


object CheckersAutoPlayer {

	def brainToPlayer(brain: CheckersBrain): (Board, Color) => Board = {
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


object CheckersTest {

	def buildBoard(pieces: Map[Piece, List[Int]]): Board = {
		val p = for ((piece, list) <- pieces.toList; pos <- list) yield ((piece, Pos.posAt(pos).get))
		val res = p.foldLeft(Board.empty) { (board, elt) => board.place(elt._1, elt._2).get }
		res
	}

}
