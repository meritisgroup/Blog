package chess

import Pos.posAt


case class Board(pieces: Map[Pos, Piece], hash: Long) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

	def contains(at: Pos): Boolean = pieces contains at

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

	def take(orig: Pos, dest: Pos): Option[Board] = {
		if ((pieces contains dest) && (pieces contains orig)) {
			pieces get orig map { piece =>
				copy(pieces = pieces - orig + ((dest, piece)),
						hash = {
							val hash1 = updateHash(hash, orig, None)
							updateHash(hash1, dest, Some(piece))
						} )
			}

		} else None
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
		ZobristHashChess.replace(acc, pos, pieces.get(pos), piece)
	}

	/*def evaluate(refColor: Color): Double = {
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
	}*/


}

object Board {

	def init: Board = {
		val backRank = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

		val items = for (y <- Seq(1, 2, 7, 8); x <- 1 to 8)
					yield (posAt(x, y) map { pos => y match {
						case 1 => (pos, Piece(White, backRank(x-1)))
						case 2 => (pos, Piece(White, Pawn))
						case 7 => (pos, Piece(Black, Pawn))
						case 8 => (pos, Piece(Black, backRank(x-1)))
					}})

		val map = items.flatten.toMap
		Board(map, ZobristHashChess.computeHash(map))
	}

	def empty: Board = {
		val map = Map.empty[Pos, Piece]
		Board(map, ZobristHashChess.computeHash(map))
	}

}
