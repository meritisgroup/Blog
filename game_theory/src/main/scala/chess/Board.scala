package chess

import chess._
import Pos.posAt


case class Board(pieces: Map[Pos, Piece]) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

	def place(piece: Piece, at: Pos): Option[Board] = {
		if (pieces contains at) None
		else Some(copy(pieces = pieces + ((at, piece))))
	}

	def remove(at: Pos): Option[Board] = pieces get at map { piece => copy(pieces = pieces - at) }

	def move(orig: Pos, dest: Pos): Option[Board] = {
		if (pieces contains dest) None
		else pieces get orig map { piece => copy(pieces = pieces - orig + ((dest, piece))) }
	}

	def take(orig: Pos, dest: Pos): Option[Board] = {
		if (pieces contains dest) pieces get orig map { piece => copy(pieces = pieces - orig + ((dest, piece))) }
		else None
	}

	def evaluate(refColor: Color): Double = {
		def evaluateEach(pos: Pos, piece: Piece): Double = piece.role match {
			case Pawn => 1
			case Rook => 5
			case Bishop => 3
			case Knight => 3
			case Queen => 9
			case _ => 0
		}

		pieces.foldLeft(0.0)((acc, elt) => {
			val eltColor = elt._2.color
			if (eltColor == refColor) acc + evaluateEach(elt._1, elt._2)
			else acc - evaluateEach(elt._1, elt._2)
		})
	}


	def check(refColor: Color): Boolean = false

	def mat(refColor: Color): Boolean = false

	def pat(refColor: Color): Boolean = false

}

object Board {

	def init: Board = {
		val backRank = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

		val items = for (y <- Seq(1, 2, 7, 8); x <- 1 to 8)
					yield (posAt(x, y) map { pos => y match {
						case 1 => (pos, Piece(Color.White, backRank(x-1)))
						case 2 => (pos, Piece(Color.White, Pawn))
						case 7 => (pos, Piece(Color.Black, Pawn))
						case 8 => (pos, Piece(Color.Black, backRank(x-1)))
					}})

		Board(items.flatten.toMap)
	}

	def empty: Board = Board(Map.empty[Pos, Piece])

}
