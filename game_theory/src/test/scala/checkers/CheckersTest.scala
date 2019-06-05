package checkers

import org.scalatest.FunSuite


abstract class CheckersTest extends FunSuite {

	def buildBoard(pieces: Map[Piece, List[Int]]): Board = {
		val p = for ((piece, list) <- pieces.toList; pos <- list) yield ((piece, Pos.posAt(pos).get))
		val res = p.foldLeft(Board.empty) { (board, elt) => board.place(elt._1, elt._2).get }
		res
	}

}
