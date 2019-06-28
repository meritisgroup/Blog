package checkers

import scala.annotation.tailrec


sealed trait Status

case object Won extends Status {
	override val hashCode = 1
}

case object Lost extends Status {
	override val hashCode = 1
}

case object Ongoing extends Status {
	override val hashCode = 1
}


case class Move(piece: Piece, from: Pos, to: Pos, captureCount: Int, after: Board)


class Moves(val current: Board, val sideToPlay: Color) {

	lazy val win: Status = computeStatus
	lazy val nextBoards: List[Board] = legalMoves map { move => move.after }

	val opponentSide = !sideToPlay

	def computeStatus: Status = {
		val countSideToPlay = current.pieces.foldLeft(0) { (acc, elt) => if (elt._2.color == sideToPlay) acc + 1 else acc }
		val countOpponent = current.pieces.foldLeft(0) { (acc, elt) => if (elt._2.color != sideToPlay) acc + 1 else acc }

		if (countSideToPlay == 0) Lost
		else if (countOpponent == 0) Won
		else if (nextBoards.isEmpty) Lost
		else Ongoing
	}

	def pawnMove(from: Pos, piece: Piece, dir: Direction): Option[Move] = dir(from) match {
		case Some(to) if !current.contains(to) =>
			current.move(from, to) map { board => Move(piece, from, to, 0, board) }
		case _ => None
	}

	@tailrec
	final def queenMove(from: Pos, pos: Pos, piece: Piece, dir: Direction, list: List[Move] = Nil): List[Move] =
		dir(pos) match {
			case Some(to) if !current.contains(to) => {
				val move = current.move(from, to) map { board => Move(piece, from, to, 0, board) }
				move match {
					case Some(m) => queenMove(from, m.to, piece, dir, m :: list)
					case _ => list
				}
			}
			case _ => list
		}

	def pawnCapture(from: Pos, pos: Pos, piece: Piece, currentBoard: Board, currentCaptures: Int, dir: Direction): List[Move] = {
		def checkEmpty(posOpponent: Pos): List[Move] = dir(posOpponent) match {
			case Some(last) if !current.contains(last) => createMove(posOpponent, last)
			case _ => Nil
		}

		def createMove(posOpponent: Pos, posEmpty: Pos): List[Move] =
			currentBoard.take(pos, posEmpty, posOpponent) match {
				case Some(nextBoard) => {
					val furtherMoves = dir.perpendiculars flatMap { d => pawnCapture(from, posEmpty, piece, nextBoard, currentCaptures + 1, d) }
					if (!furtherMoves.isEmpty) furtherMoves
					else List(Move(piece, from, posEmpty, currentCaptures + 1, nextBoard))
				}
				case _ => Nil
			}

		dir(pos) match {
			case Some(to) if current.contains(to) && current(to).get.is(opponentSide) => checkEmpty(to)
			case _ => Nil
		}
	}

	def queenCapture(from: Pos, pos: Pos, piece: Piece, currentBoard: Board, currentCaptures: Int, allowedDirs: List[Direction]): List[Move] = {
		@tailrec
		def firstOpponent(p: Pos, dir: Direction): Option[Pos] = dir(p) match {
			case Some(to) => {
				if (current(to).isEmpty) firstOpponent(to, dir)
				else if (current(to).get.is(!piece.color)) Some(to)
				else None
			}
			case _ => None
		}

		@tailrec
		def emptyPosAfter(p: Pos, dir: Direction, list: List[Pos] = Nil): List[Pos] = dir(p) match {
			case Some(to) if current(to).isEmpty => emptyPosAfter(to, dir, to :: list)
			case _ => list
		}

		def buildMove(posOpponent: Pos, posEmpty: Pos): Option[Move] = {
			currentBoard.take(pos, posEmpty, posOpponent) map { board =>
				Move(piece, from, posEmpty, currentCaptures + 1, board)
			}
		}

		allowedDirs flatMap { dir =>
			firstOpponent(pos, dir) match {
				case Some(posOpponent) => {
					val allEmptyPos = emptyPosAfter(posOpponent, dir)
					val moves = ( allEmptyPos map { posEmpty => buildMove(posOpponent, posEmpty) } ).flatten
					val furtherMoves = moves flatMap { move => queenCapture(from, move.to, piece, move.after, move.captureCount, dir.perpendiculars) }

					if (!furtherMoves.isEmpty) furtherMoves
					else moves
				}
				case _ => Nil
			}
		}
	}

	def whitePromoteFilter(pos: Pos): Boolean = (pos.m <= 5)
	def blackPromoteFilter(pos: Pos): Boolean = (pos.m >= 46)

	val promoteFilter: Pos => Boolean = if (sideToPlay == White) whitePromoteFilter else blackPromoteFilter

	def promote(move: Move): Move = {
		if (!promoteFilter(move.to)) {
			move
		} else {
			val newBoard = move.after.replace(Piece(sideToPlay, Queen), move.to)
			move.copy(after = newBoard.get)
		}
	}

	def promoteAll(moves: List[Move]): List[Move] = moves map promote

	def legalMoves: List[Move] = {

		def dispatchCapture(pos: Pos, piece: Piece): List[Move] = piece.role match {
			case Pawn => promoteAll(Direction.all flatMap { dir => pawnCapture(pos, pos, piece, current, 0, dir) })
			case Queen => queenCapture(pos, pos, piece, current, 0, Direction.all)
 		}

		def dispatchMove(pos: Pos, piece: Piece): List[Move] = piece match {
			case Piece(White, Pawn) => promoteAll(Direction.whitePawnMoves flatMap { dir => pawnMove(pos, piece, dir) })
			case Piece(Black, Pawn) => promoteAll(Direction.blackPawnMoves flatMap { dir => pawnMove(pos, piece, dir) })
			case Piece(_, Queen) => Direction.all flatMap { dir => queenMove(pos, pos, piece, dir) }
 		}

		val allCapture = current.pieces flatMap { case (pos, piece) => if (piece is sideToPlay) dispatchCapture(pos, piece) else Nil }

		if (allCapture.isEmpty) {
			val moves = current.pieces flatMap { case (pos, piece) => if (piece is sideToPlay) dispatchMove(pos, piece) else Nil }
			moves.toList

		} else {
			val maxCaptureCount = allCapture.maxBy { move => move.captureCount }.captureCount
			allCapture.filter(move => move.captureCount == maxCaptureCount).toList
		}
	}

}


object Moves {

	def computeHash(current: Board, sideToPlay: Color): Long = {
		ZobristHashCheckers.addSide(current.hash, sideToPlay)
	}

}
