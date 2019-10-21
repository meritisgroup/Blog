package chess

import scala.annotation.tailrec
import chess._


case class Move(piece: Piece, origin: Pos, dest: Pos, after: Board, afterHistory: History)


class Moves(val current: Board, val currentHistory: History, val sideToPlay: Color) {

	lazy val gameStatus: Option[Status] = computeStatus
	lazy val nextBoards: List[Board] = legalMoves map { move => move.after }

	val opponentSide = !sideToPlay

	def computeStatus: Option[Status] = {
		None
	}

	def legalMoves: List[Move] = {
		current.pieces flatMap {
			case (pos, piece) => piece.role match {
				case Bishop => longRange(piece, pos, Bishop.dirs)
				case Knight => shortRange(piece, pos, Knight.dirs)
				case Rook => rookMovedNoCastle(longRange(piece, pos, Rook.dirs))
				case Queen => longRange(piece, pos, Queen.dirs)
				case King => kinkMovedNoCastle(shortRange(piece, pos, King.dirs)) ++ castle(piece, pos)
				case Pawn => promoteAll(pawnMoves(piece, pos))
			}
		} toList
	}

	def longRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Move]): List[Move] = dir(from) match {
			case Some(to) if !current.contains(to) => {
				val newMove = current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
				if (newMove.isEmpty) list else rec(to, dir, newMove.get :: list)
			}
			case Some(to) if current.contains(to) && !current(to).get.is(piece.color) => {
				val newMove = current.take(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
				if (newMove.isEmpty) list else newMove.get :: list
			}
			case _ => list
		}

		dirs.flatMap { dir => rec(pos, dir, Nil) }
	}

	def shortRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		dirs flatMap { dir => dir(pos) } flatMap { to =>
			if (!current.contains(to)) {
				current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
			} else if (!current(to).get.is(piece.color)) {
				current.take(pos, to) map { after => Move(piece, pos, to, after, currentHistory)}
			} else Nil
		}
	}

	def pawnMoves(piece: Piece, pos: Pos): List[Move] = {
		def firstLine(y: Int): Boolean = {
			if (piece.is(White)) y == 2 else y == 7
		}

		val movingDir: Direction = if (piece.is(White)) (_.up) else (_.down)

		def moveBy2(from: Pos): Option[Move] = movingDir(from) match {
			case Some(to) if !current.contains(to) =>
				current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
			case _ => None
		}

		def moveBy1or2: List[Move] = movingDir(pos) match {
			case Some(to) if !current.contains(to) => {
				val c1 = current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
				if (c1.isEmpty) Nil
				else if (!firstLine(pos.y)) List(c1.get)
				else List(c1, moveBy2(to)).flatten
			}
			case _ => Nil
		}

		def takeCross(dir: Direction): Option[Move] = dir(pos) match {
			case Some(to) if current.contains(to) && !current(to).get.is(piece.color) =>
				current.take(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
			case _ => None
		}

		def taken: List[Move] = Moves.pawnTakingDirs(piece.color) flatMap { dir => takeCross(dir) }

		moveBy1or2 ++ taken
	}

	def promote(move: Move): Move = {
		def promoteFilter(pos: Pos): Boolean = {
			if (sideToPlay == White) pos.y == 8 else pos.y == 1
		}

		if (!promoteFilter(move.dest)) {
			move
		} else {
			val newBoard = move.after.replace(Piece(sideToPlay, Queen), move.dest)
			move.copy(after = newBoard.get)
		}
	}

	def promoteAll(moves: List[Move]): List[Move] = moves map promote

	def rookMovedNoCastle(moves: List[Move]): List[Move] = {
		moves map { move => 
			val newHistory = move.afterHistory.rookMoved(sideToPlay, move.origin)
			move.copy(afterHistory = newHistory)
		}
	}

	def kinkMovedNoCastle(moves: List[Move]): List[Move] = {
		moves map { move => 
			val newHistory = move.afterHistory.kingMoved(sideToPlay)
			move.copy(afterHistory = newHistory)
		}
	}

	def castle(piece: Piece, pos: Pos): List[Move] = {
		def shortCastle: Option[Move] = {
			val kingDest = Pos.posAt(7, pos.y).get
			val rookOrigin = Pos.posAt(8, pos.y).get
			val rookDest = Pos.posAt(6, pos.y).get

			if (currentHistory.allowed(sideToPlay, false)
					&& !current.contains(kingDest) && !current.contains(rookDest)) {

				for (b1 <- current.move(pos, kingDest); after <- b1.move(rookOrigin, rookDest))
				yield Move(piece, pos, kingDest, after, currentHistory.kingMoved(sideToPlay))

			} else None
		}

		def longCastle: Option[Move] = {
			val kingDest = Pos.posAt(3, pos.y).get
			val rookOrigin = Pos.posAt(1, pos.y).get
			val rookDest = Pos.posAt(4, pos.y).get
			val temp = Pos.posAt(2, pos.y).get

			if (currentHistory.allowed(sideToPlay, true)
					&& !current.contains(kingDest) && !current.contains(rookDest) && !current.contains(temp)) {

				for (b1 <- current.move(pos, kingDest); after <- b1.move(rookOrigin, rookDest))
				yield Move(piece, pos, kingDest, after, currentHistory.kingMoved(sideToPlay))
			
			} else None
		}

		List(shortCastle, longCastle).flatten
	}

}


object Moves {

	val pawnTakingDirs: Map[Color,Directions] = Map(White -> List(_.upLeft, _.upRight), Black -> List(_.downLeft, _.downRight))

	def computeHash(current: Board, sideToPlay: Color): Long = {
		ZobristHashChess.addSide(current.hash, sideToPlay)
	}

}
