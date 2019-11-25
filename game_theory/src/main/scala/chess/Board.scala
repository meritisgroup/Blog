package chess

import Pos.posAt


case class Board(pieces: Map[Pos, Piece], hash: Long) {

	def apply(at: Pos): Option[Piece] = pieces get at

	def apply(x: Int, y: Int): Option[Piece] = posAt(x, y) flatMap pieces.get

	def contains(at: Pos): Boolean = pieces contains at

	lazy val kingPos: Map[Color, Pos] = pieces.collect {
		case (pos, Piece(color, King)) => color -> pos
	}

	def findKingPos(color: Color): Option[Pos] = kingPos get color

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

	override def hashCode: Int = hash.hashCode

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
