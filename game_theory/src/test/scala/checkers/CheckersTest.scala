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


object CheckersTest {

	def buildBoard(pieces: Map[Piece, List[Int]]): Board = {
		val p = for ((piece, list) <- pieces.toList; pos <- list) yield ((piece, Pos.posAt(pos).get))
		val res = p.foldLeft(Board.empty) { (board, elt) => board.place(elt._1, elt._2).get }
		res
	}

}
