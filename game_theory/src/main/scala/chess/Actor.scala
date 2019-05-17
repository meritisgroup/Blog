package chess

import scala.annotation.tailrec
import chess._


case class Actor(piece: Piece, pos: Pos, board: Board) {
	
	lazy val moves: List[Move] = trustedMoves

	def trustedMoves: List[Move] = piece.role match {
		case Bishop => longRange(Bishop.dirs)
		case Knight => shortRange(Knight.dirs)
		case Rook => longRange(Rook.dirs)
		case Queen => longRange(Queen.dirs)
		case King => shortRange(King.dirs)
		case Pawn => pawnMoves
	}

	def longRange(dirs: Directions): List[Move] = {
		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Option[Move]]): List[Option[Move]] = dir(from) match {
			case Some(to) if board(to).isEmpty => {
				val newMove = board.move(pos, to) map { after => Move(piece, pos, to, after) }
				rec(to, dir, newMove :: list)
			}
			case Some(to) if !board(to).isEmpty && !board(to).get.is(piece.color) => {
				val newMove = board.take(pos, to) map { after => Move(piece, pos, to, after) }
				newMove :: list
			}
			case _ => list
		}

		dirs flatMap { dir => rec(pos, dir, Nil) } flatten
	}

	def shortRange(dirs: Directions): List[Move] = {
		dirs flatMap { dir => dir(pos) } flatMap { to =>
			if (board(to).isEmpty) {
				board.move(pos, to) map { after => Move(piece, pos, to, after) }
			} else if (!board(to).get.is(piece.color)) {
				board.take(pos, to) map { after => Move(piece, pos, to, after)}
			} else Nil
		}
	}

	def pawnMoves: List[Move] = {
		val firstLine = if (piece.is(Color.White)) 2 else 7
		val movingDir: Direction = if (piece.is(Color.White)) (_.up) else (_.down)
		val takingDirs: Directions = if (piece.is(Color.White)) List(_.upLeft, _.upRight)
										else List(_.downLeft, _.downRight)

		def moveBy2(from: Pos): Option[Move] = movingDir(from) match {
			case Some(to) if pos.y == firstLine && board(to).isEmpty =>
				board.move(pos, to) map { after => Move(piece, pos, to, after) }
			case _ => None
		}

		def moveBy1or2(from: Pos): List[Move] = movingDir(from) match {
			case Some(to) if board(to).isEmpty => {
				val c1 = board.move(pos, to) map { after => Move(piece, pos, to, after) }
				if (c1.isEmpty) Nil
				else List(c1, moveBy2(to)).flatten
			}
			case _ => Nil
		}

		def takeCross(from: Pos, dir: Direction): Option[Move] = dir(pos) match {
			case Some(to) if !board(to).isEmpty =>
				board.take(pos, to) map { after => Move(piece, pos, to, after) }
			case _ => None
		}

		moveBy1or2(pos) ++ (takingDirs flatMap { dir => takeCross(pos, dir) })
	}

}
