package chess

import scala.annotation.tailrec
import chess._


case class Move(piece: Piece, origin: Pos, dest: Pos, after: Board, afterHistory: History)


class Moves(val current: Board, val currentHistory: History, val sideToPlay: Color) {

	lazy val gameStatus: Option[Status] = computeStatus
	lazy val nextBoards: List[Board] = legalMoves map { move => move.after }

	val kingInCheck: Boolean = Moves.kingThreatened(current, sideToPlay)

	val opponentSide = !sideToPlay

	def computeStatus: Option[Status] = {
		if (!nextBoards.isEmpty) None
		else if (kingInCheck) Some(Mate)
		else Some(Draw)
	}

	def legalMoves: List[Move] = {
		current.pieces
			.collect { case (pos, piece) if piece is sideToPlay =>
				piece.role match {
					case Bishop => safeKingFilter(longRange(piece, pos, Bishop.dirs))
					case Knight => safeKingFilter(shortRange(piece, pos, Knight.dirs))
					case Rook => rookMovedNoCastle(safeKingFilter(longRange(piece, pos, Rook.dirs)))
					case Queen => safeKingFilter(longRange(piece, pos, Queen.dirs))
					case King => safeKingFilter(kinkMovedNoCastle(shortRange(piece, pos, King.dirs))) ++ castle(piece, pos)
					case Pawn => promoteAll(safeKingFilter(pawnMoves(piece, pos)))
				}}
			.flatten
			.toList
	}

	def longRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Move] = Nil): List[Move] = dir(from) match {
			case Some(to) => {
				if (!current.contains(to)) {
					current.move(pos, to).map(after => Move(piece, pos, to, after, currentHistory)) match {
						case Some(move) => rec(to, dir, move :: list)
						case _ => list
					}

				} else if (!current(to).get.is(piece.color)) {
					current.take(pos, to).map(after => Move(piece, pos, to, after, currentHistory)) match {
						case Some(move) => move :: list
						case _ => list
					}

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
		moves map { move =>
			move.copy(afterHistory = move.afterHistory.rookMoved(sideToPlay, move.origin))
		}
	}

	def kinkMovedNoCastle(moves: List[Move]): List[Move] = {
		moves map { move =>
			move.copy(afterHistory = move.afterHistory.kingMoved(sideToPlay))
		}
	}

	def castle(piece: Piece, pos: Pos): List[Move] = {
		val shortAllowed = currentHistory.allowed(sideToPlay, false)
		val longAllowed = currentHistory.allowed(sideToPlay, true)

		def stopCheck(ref: Pos): Boolean = {
			current.contains(ref) || Moves.posThreatened(current, sideToPlay, ref)
		}

		def stopContains(ref: Pos): Boolean = current.contains(ref)

		def shortCastle: Option[Move] = {
			val kingDest = Pos.posAt(7, pos.y).get

			if (Pos.checkAll(pos, kingDest, stopCheck)) {
				val rookOrigin = Pos.posAt(8, pos.y).get
				val rookDest = Pos.posAt(6, pos.y).get

				for (b1 <- current.move(pos, kingDest); after <- b1.move(rookOrigin, rookDest))
				yield Move(piece, pos, kingDest, after, currentHistory.kingMoved(sideToPlay))

			} else None
		}

		def longCastle: Option[Move] = {
			val kingDest = Pos.posAt(3, pos.y).get
			val rookOrigin = Pos.posAt(1, pos.y).get
			val rookDest = Pos.posAt(4, pos.y).get

			if (Pos.checkAll(pos, kingDest, stopCheck) && Pos.checkAll(rookOrigin, rookDest, stopContains)) {
				for (b1 <- current.move(pos, kingDest); after <- b1.move(rookOrigin, rookDest))
				yield Move(piece, pos, kingDest, after, currentHistory.kingMoved(sideToPlay))

			} else None
		}

		if (!shortAllowed && !longAllowed) Nil
		else if (kingInCheck) Nil
		else List(if (!shortAllowed) None else shortCastle,
					if (!longAllowed) None else longCastle).flatten
	}

	def safeKingFilter(moves: List[Move]): List[Move] = {
		moves.filter(move => !Moves.kingThreatened(move.after, sideToPlay))
	}

}


object Moves {

	def computeHash(current: Board, sideToPlay: Color): Long = {
		ZobristHashChess.addSide(current.hash, sideToPlay)
	}

	def kingThreatened(board: Board, color: Color): Boolean = {
		val kingPos = board.findKingPos(color)
		if (kingPos.isEmpty) false
		else posThreatened(board, color, kingPos.get)
	}

	def posThreatened(board: Board, color: Color, ref: Pos): Boolean = {
		def pieceThreatenRef(pos: Pos, piece: Piece): Boolean = piece.role match {
			case King if Pos.distance2(pos, ref) <= 2 => true
			case Bishop if ref.onSameDiagonal(pos) => Pos.checkAll(pos, ref, pos => board.contains(pos))
			case Rook if ref.onSameLine(pos) => Pos.checkAll(pos, ref, pos => board.contains(pos))
			case Queen if (ref.onSameLine(pos) || ref.onSameDiagonal(pos)) => Pos.checkAll(pos, ref, pos => board.contains(pos))
			case Knight if Pos.distance2(pos, ref) == 5 => true
			case Pawn if Pos.distance2(pos, ref) == 2 => piece.pawnTakingDirs.exists(dir => dir(pos) == Some(ref))
			case _ => false
		}

		board.pieces.exists(elt => (elt._2 is !color) && pieceThreatenRef(elt._1, elt._2))
	}

}
