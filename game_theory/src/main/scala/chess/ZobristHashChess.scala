package chess

import scala.util.Random


object ZobristHashChess {

	private final val rand = new Random(1)

	final val zobrist: Map[(Pos, Option[Piece]), Long] = {
		val allPieces = None :: Piece.all.map(p => Some(p))

		val seq = for (pos <- Pos.all; piece <- allPieces) yield ((pos, piece) -> rand.nextLong())
		Map(seq: _*)
	}

	final val zobrist2: Map[Color, Long] = {
		val seq = for (side <- List(White, Black)) yield (side -> rand.nextLong())
		Map(seq: _*)
	}

	def computeHash(map: Map[Pos, Piece]): Long = {
		val longs = for (pos <- Pos.all) yield zobrist((pos, map.get(pos)))
		longs.foldLeft(0L) { (acc, elt) => acc ^ elt }
	}

	def replace(prevHash: Long, pos: Pos, oldPiece: Option[Piece], newPiece: Option[Piece]): Long = {
		prevHash ^ zobrist((pos, oldPiece)) ^ zobrist((pos, newPiece))
	}

	def addSide(prevHash: Long, side: Color): Long = {
		prevHash ^ zobrist2(side)
	}

}
