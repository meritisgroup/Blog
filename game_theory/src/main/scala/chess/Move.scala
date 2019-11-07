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
				case Bishop => safeKingFilter(longRange(piece, pos, Bishop.dirs))
				case Knight => safeKingFilter(shortRange(piece, pos, Knight.dirs))
				case Rook => rookMovedNoCastle(safeKingFilter(longRange(piece, pos, Rook.dirs)))
				case Queen => safeKingFilter(longRange(piece, pos, Queen.dirs))
				case King => safeKingFilter(kinkMovedNoCastle(shortRange(piece, pos, King.dirs)) ++ castle(piece, pos))
				case Pawn => promoteAll(safeKingFilter(pawnMoves(piece, pos)))
			}
		} toList
	}

	def longRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Move] = Nil): List[Move] = dir(from) match {
			case Some(to) => {
				if (!current.contains(to)) {
					val newMove = current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
					if (newMove.isEmpty) list else rec(to, dir, newMove.get :: list)

				} else if (!current(to).get.is(piece.color)) {
					val newMove = current.take(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
					if (newMove.isEmpty) list else newMove.get :: list

				} else {
					list
				}
			}
			case _ => list
		}

		dirs flatMap { dir => rec(pos, dir) }
	}

	def shortRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		dirs flatMap { dir =>
			dir(pos) match {
				case Some(to) => {
					if (!current.contains(to)) {
						current.move(pos, to) map { after => Move(piece, pos, to, after, currentHistory) }
					} else if (!current(to).get.is(piece.color)) {
						current.take(pos, to) map { after => Move(piece, pos, to, after, currentHistory)}
					} else Nil
				}
				case _ => Nil
			}
		}
	}

	def firstPawnLine(pos: Pos): Boolean = {
		(sideToPlay == White && pos.y == 2) || (sideToPlay == Black && pos.y == 7)
	}

	def lastPawnLine(pos: Pos): Boolean = {
		(sideToPlay == White && pos.y == 8) || (sideToPlay == Black && pos.y == 1)
	}

	def pawnMoves(piece: Piece, pos: Pos): List[Move] = {
		val moveBy1: Option[Move] = piece.pawnMovingDir(pos) match {
			case Some(to) if !current.contains(to) =>
				current.move(pos, to) map { after =>
					Move(piece, pos, to, after, currentHistory.resetEnPassant)
				}
			case _ => None
		}

		val moveBy2: Option[Move] = {
			if (!moveBy1.isEmpty && firstPawnLine(pos)) {
				piece.pawnMovingDir(moveBy1.get.dest) match {
					case Some(to) if !current.contains(to) =>
						current.move(pos, to) map { after =>
							Move(piece, pos, to, after, currentHistory.pawnMovedBy2(piece.color, pos.x))
						}
					case _ => None
				}
			} else None
		}

		def takeCross(dir: Direction): Option[Move] = dir(pos) match {
			case Some(to) if current.contains(to) && !current(to).get.is(piece.color) =>
				current.take(pos, to) map { after =>
					Move(piece, pos, to, after, currentHistory.resetEnPassant)
				}
			case _ => None
		}

		val takeEnPassant: Option[Move] = {
			if (currentHistory.allowed(piece.color, pos)) {
				val taken = Pos.posAt(currentHistory.enPassant.get.x, if (piece.color == White) 5 else 4)
				current.take(pos, currentHistory.enPassant.get, taken.get) map { after =>
					Move(piece, pos, currentHistory.enPassant.get, after, currentHistory.resetEnPassant)
				}
			} else None
		}

		(moveBy1 :: moveBy2 :: takeEnPassant :: (piece.pawnTakingDirs map takeCross)).flatten
	}

	def promoteAll(moves: List[Move]): List[Move] = {
		def promote(move: Move): Move = {
			if (!lastPawnLine(move.dest)) {
				move
			} else {
				val newBoard = move.after.replace(Piece(sideToPlay, Queen), move.dest)
				move.copy(after = newBoard.get)
			}
		}

		moves map promote
	}

	def rookMovedNoCastle(moves: List[Move]): List[Move] = {
		def update(move: Move): Move = {
			move.copy(afterHistory = move.afterHistory.rookMoved(sideToPlay, move.origin))
		}
		moves map update
	}

	def kinkMovedNoCastle(moves: List[Move]): List[Move] = {
		def update(move: Move): Move = {
			move.copy(afterHistory = move.afterHistory.kingMoved(sideToPlay))
		}
		moves map update
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

	def safeKingFilter(moves: List[Move]): List[Move] = {
		def findDir(from: Pos, to: Pos): Option[Direction] = {
			val dx = if (to.x == from.x) 0 else if (to.x > from.x) 1 else -1
			val dy = if (to.y == from.y) 0 else if (to.y > from.y) 1 else -1

			(dx, dy) match {
				case (1, 1) =>   Some(_.upRight)
				case (1, -1) =>  Some(_.downRight)
				case (-1, 1) =>  Some(_.upLeft)
				case (-1, -1) => Some(_.downLeft)
				case (1, 0) => Some(_.right)
				case (-1, 0) => Some(_.left)
				case (0, 1) => Some(_.up)
				case (0, -1) => Some(_.down)
				case _ => None
			}
		}

		def findKingPos(board: Board): Option[Pos] = {
			val king = board.pieces.find(elt => elt._2.role == King && (elt._2 is sideToPlay))
			king map (elt => elt._1)
		}

		def kingThreatened(board: Board, kingPos: Pos): Boolean = {
			@tailrec
			def emptyInBetween(from: Pos, to: Pos, dir: Direction): Boolean = dir(from) match {
				case Some(pos) if pos == to => true
				case Some(pos) if !board.contains(pos) => emptyInBetween(pos, to, dir)
				case _ => false
			}

			def pieceThreatenKing(pos: Pos, piece: Piece): Boolean = piece.role match {
				case King if Pos.distance2(pos, kingPos) <= 2 => true
				case Bishop if kingPos.onSameDiagonal(pos) => findDir(pos, kingPos)
																.exists(dir => emptyInBetween(pos, kingPos, dir))
				case Rook if kingPos.onSameLine(pos) => findDir(pos, kingPos)
															.exists(dir => emptyInBetween(pos, kingPos, dir))
				case Queen if (kingPos.onSameLine(pos) || kingPos.onSameDiagonal(pos)) => findDir(pos, kingPos)
																							.exists(dir => emptyInBetween(pos, kingPos, dir))
				case Knight if Pos.distance2(pos, kingPos) == 5 => true
				case Pawn if Pos.distance2(pos, kingPos) == 2 => piece.pawnTakingDirs.exists(dir => dir(pos) == kingPos)
				case _ => false
			}

			board.pieces.exists(elt => (elt._2 is !sideToPlay) && pieceThreatenKing(elt._1, elt._2))
		}

		moves.filterNot(move => findKingPos(move.after).exists(kingPos => kingThreatened(move.after, kingPos)))
	}

}


object Moves {

	def computeHash(current: Board, sideToPlay: Color): Long = {
		ZobristHashChess.addSide(current.hash, sideToPlay)
	}

}
