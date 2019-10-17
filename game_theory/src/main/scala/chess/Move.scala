package chess

import scala.annotation.tailrec
import chess._


case class Move(piece: Piece, origin: Pos, dest: Pos, after: Board)


class Moves(val current: Board, val sideToPlay: Color) {

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
				case Rook => longRange(piece, pos, Rook.dirs)
				case Queen => longRange(piece, pos, Queen.dirs)
				case King => shortRange(piece, pos, King.dirs)
				case Pawn => pawnMoves(piece, pos)
			}
		} toList
	}

	def longRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Option[Move]]): List[Option[Move]] = dir(from) match {
			case Some(to) if current(to).isEmpty => {
				val newMove = current.move(pos, to) map { after => Move(piece, pos, to, after) }
				rec(to, dir, newMove :: list)
			}
			case Some(to) if !current(to).isEmpty && !current(to).get.is(piece.color) => {
				val newMove = current.take(pos, to) map { after => Move(piece, pos, to, after) }
				newMove :: list
			}
			case _ => list
		}

		val result = dirs.flatMap { dir => rec(pos, dir, Nil) }
		result.flatten
	}

	def shortRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		dirs flatMap { dir => dir(pos) } flatMap { to =>
			if (current(to).isEmpty) {
				current.move(pos, to) map { after => Move(piece, pos, to, after) }
			} else if (!current(to).get.is(piece.color)) {
				current.take(pos, to) map { after => Move(piece, pos, to, after)}
			} else Nil
		}
	}

	def pawnMoves(piece: Piece, pos: Pos): List[Move] = {
		val firstLine = if (piece.is(White)) 2 else 7
		val movingDir: Direction = if (piece.is(White)) (_.up) else (_.down)
		val takingDirs: Directions = if (piece.is(White)) List(_.upLeft, _.upRight)
										else List(_.downLeft, _.downRight)

		def moveBy2(from: Pos): Option[Move] = movingDir(from) match {
			case Some(to) if pos.y == firstLine && current(to).isEmpty =>
				current.move(pos, to) map { after => Move(piece, pos, to, after) }
			case _ => None
		}

		def moveBy1or2(from: Pos): List[Move] = movingDir(from) match {
			case Some(to) if current(to).isEmpty => {
				val c1 = current.move(pos, to) map { after => Move(piece, pos, to, after) }
				if (c1.isEmpty) Nil
				else List(c1, moveBy2(to)).flatten
			}
			case _ => Nil
		}

		def takeCross(from: Pos, dir: Direction): Option[Move] = dir(pos) match {
			case Some(to) if !current(to).isEmpty =>
				current.take(pos, to) map { after => Move(piece, pos, to, after) }
			case _ => None
		}

		moveBy1or2(pos) ++ (takingDirs flatMap { dir => takeCross(pos, dir) })
	}

}


object Moves {

	def computeHash(current: Board, sideToPlay: Color): Long = {
		ZobristHashChess.addSide(current.hash, sideToPlay)
	}

}
