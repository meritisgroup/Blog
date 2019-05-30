package checkers

import Pos._


case class Board(pieces: Map[Pos, Piece]) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def place(piece: Piece, at: Pos): Option[Board] = {
		if (pieces contains at) None
		else Some(copy(pieces = pieces + ((at, piece))))
	}

	def remove(at: Pos): Option[Board] = pieces get at map { piece => copy(pieces = pieces - at) }

	def move(orig: Pos, dest: Pos): Option[Board] = {
		if (pieces contains dest) None
		else pieces get orig map { piece => copy(pieces = pieces - orig + ((dest, piece))) }
	}

	def take(orig: Pos, dest: Pos, taken: Pos): Option[Board] = {
		if ((pieces contains taken) && !(pieces contains dest)) pieces get orig map { piece => copy(pieces = pieces - orig - taken + ((dest, piece))) }
		else None
	}

	def evaluate(refColor: Color): Double = {
		def evaluateEach(pos: Pos, piece: Piece): Double = piece.role match {
			case Pawn => 1
			case Queen => 3
		}

		pieces.foldLeft(0.0)((acc, elt) => {
			val eltColor = elt._2.color
			if (eltColor == refColor) acc + evaluateEach(elt._1, elt._2)
			else acc - evaluateEach(elt._1, elt._2)
		})
	}

}


object Board {

	def init: Board = {
		val blackPart = for (m <- 1 to 20) yield (posAt(m).get, Piece(Black, Pawn))
		val whitePart = for (m <- 31 to 50) yield (posAt(m).get, Piece(White, Pawn))

		Board((blackPart ++ whitePart).toMap)
	}

	def empty: Board = Board(Map.empty[Pos, Piece])

}
