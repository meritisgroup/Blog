package checkers

import org.scalatest.FunSuite


abstract class CheckersTest extends FunSuite {

	def buildBoard(pieces: Map[Piece, List[Int]]): Board = {
		val p = for ((piece, list) <- pieces.toList; pos <- list) yield ((piece, Pos.posAt(pos).get))
		val res = p.foldLeft(Board.empty) { (board, elt) => board.place(elt._1, elt._2).get }
		res
	}

	def autoPlay(side: Color, board: Board, player1: CheckersBrain, player2: CheckersBrain,
					log: Boolean = true, list: List[Move] = Nil): (Option[Color], List[Move]) = {

		if (list.size >= 15) {
			(None, list.reverse)
		} else {
			val result = player1.bestMove(board)

			val move = result._1.get
			val nextBoard = move.after
			val status = new Moves(nextBoard, !side).win

			if (log) {
				println("Auto play : next move by " + move.piece + " " + move.from.m + " x " + move.to.m + " (capture=" + move.captureCount + ")")
				println(nextBoard + "\n")
			}

			if (status == Lost) {
				(Some(side), (move :: list).reverse)
			} else if (status == Won) {
				(None, (move :: list).reverse)
			} else {
				autoPlay(!side, nextBoard, player2, player1, log, move :: list)
			}
		}
	}

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
