package checkers

import Pos._


case class Board(pieces: Map[Pos, Piece], hash: Long) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def place(piece: Piece, at: Pos): Option[Board] = {
		if (pieces contains at) None
		else Some(copy(pieces = pieces + ((at, piece)),
						hash = updateHash(hash, at, Some(piece))))
	}

	def replace(piece: Piece, at: Pos): Option[Board] = {
		if (!(pieces contains at)) None
		else Some(copy(pieces = pieces + ((at, piece)),
						hash = updateHash(hash, at, Some(piece))))
	}

	def remove(at: Pos): Option[Board] = {
		pieces get at map { piece =>
			copy(pieces = pieces - at,
					hash = updateHash(hash, at, None))
		}
	}

	def move(orig: Pos, dest: Pos): Option[Board] = {
		if (pieces contains dest) None
		else pieces get orig map { piece =>
			copy(pieces = pieces - orig + ((dest, piece)),
					hash = { 
						val hash1 = updateHash(hash, orig, None)
						updateHash(hash1, dest, Some(piece))
					} )
		}
	}

	def take(orig: Pos, dest: Pos, taken: Pos): Option[Board] = {
		if ((pieces contains taken) && !(pieces contains dest) && (pieces contains orig)) {
			pieces get orig map { piece =>
				copy(pieces = pieces - orig - taken + ((dest, piece)),
						hash = {
							val hash1 = updateHash(hash, orig, None)
							val hash2 = updateHash(hash1, taken, None)
							updateHash(hash2, dest, Some(piece))
						} )
			}

		} else None
	}

	def updateHash(acc: Long, pos: Pos, piece: Option[Piece]): Long = {
		ZobristHashCheckers.replace(acc, pos, pieces.get(pos), piece)
	}

}


object Board {

	def init: Board = {
		val blackPart = for (m <- 1 to 20) yield (posAt(m).get, Piece(Black, Pawn))
		val whitePart = for (m <- 31 to 50) yield (posAt(m).get, Piece(White, Pawn))

		val map = (blackPart ++ whitePart).toMap
		Board(map, ZobristHashCheckers.computeHash(map))
	}

	def empty: Board = {
		val map = Map.empty[Pos, Piece]
		Board(map, ZobristHashCheckers.computeHash(map))
	}

}
