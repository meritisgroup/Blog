package checkers

import Pos._


case class Board(pieces: Map[Pos, Piece], hash: Long) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def place(piece: Piece, at: Pos): Option[Board] = {
		if (pieces contains at) None
		else Some(copy(pieces = pieces + ((at, piece)),
						hash = updateHash(hash, (at, Some(piece)))))
	}

	def replace(piece: Piece, at: Pos): Option[Board] = {
		if (!(pieces contains at)) None
		else Some(copy(pieces = pieces - at + ((at, piece)),
						hash = updateHash(hash, (at, Some(piece)))))
	}

	def remove(at: Pos): Option[Board] = {
		pieces get at map { piece =>
			copy(pieces = pieces - at,
					hash = updateHash(hash, (at, None)))
		}
	}

	def move(orig: Pos, dest: Pos): Option[Board] = {
		if (pieces contains dest) None
		else pieces get orig map { piece =>
			copy(pieces = pieces - orig + ((dest, piece)),
					hash = List((orig, None), (dest, Some(piece))).foldLeft(hash)(updateHash))
		}
	}

	def take(orig: Pos, dest: Pos, taken: Pos): Option[Board] = {
		if ((pieces contains taken) && !(pieces contains dest) && (pieces contains orig)) {
			pieces get orig map { piece =>
				copy(pieces = pieces - orig - taken + ((dest, piece)),
						hash = List((orig, None), (taken, None), (dest, Some(piece))).foldLeft(hash)(updateHash))
			}

		} else None
	}

	def updateHash(acc: Long, elt: (Pos, Option[Piece])): Long = {
		ZobristHashCheckers.replace(acc, elt._1, pieces.get(elt._1), elt._2)
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
