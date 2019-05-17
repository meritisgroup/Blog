package chess

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
		//@tailrec
		def rec(from: Pos, dir: Direction, list: List[Move]): List[Move] = dir(from) match {
			case Some(to) if board(to).isEmpty => {
				val iter = board.move(pos, to) map { after => Move(piece, pos, to, after) }
				rec(to, dir, iter.toList ++ list)
			}
			case Some(to) if !board(to).isEmpty && !board(to).get.is(piece.color) => {
				val iter = board.take(pos, to) map { after => Move(piece, pos, to, after)}
				iter.toList ++ list
			}
			case _ => list
		}

		dirs flatMap { dir => rec(pos, dir, Nil) }
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
		val dir: Direction = if (piece.is(Color.White)) (_.up) else (_.down)

		def moveBy2(from: Pos): Option[Move] = dir(from) match {
			case Some(to) if pos.y == firstLine && board(to).isEmpty => board.move(pos, to) map { after => Move(piece, pos, to, after) }
			case _ => None
		}

		def moveBy1or2(from: Pos): List[Move] = dir(from) match {
			case None => Nil
			case Some(to) if !board(to).isEmpty => Nil
			case Some(to) => {
				val c1 = board.move(pos, to) map { after => Move(piece, pos, to, after) }
				val c2 = moveBy2(to)
				List(c1, c2).flatten
			}
		}

		moveBy1or2(pos)
	}

}
