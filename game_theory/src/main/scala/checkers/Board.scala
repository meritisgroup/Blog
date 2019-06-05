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

}


object Board {

	def init: Board = {
		val blackPart = for (m <- 1 to 20) yield (posAt(m).get, Piece(Black, Pawn))
		val whitePart = for (m <- 31 to 50) yield (posAt(m).get, Piece(White, Pawn))

		Board((blackPart ++ whitePart).toMap)
	}

	def empty: Board = Board(Map.empty[Pos, Piece])

}
