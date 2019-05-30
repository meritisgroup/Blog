package checkers

import scala.annotation.tailrec


sealed trait Direction {
	def apply(at: Pos): Option[Pos]
	def perpendicular: List[Direction]
}

case object UpAndLeft extends Direction {
	override def apply(at: Pos) = at.upLeft
	override lazy val perpendicular = this :: Direction.computePerpendicular(this)
}

case object UpAndRight extends Direction {
	override def apply(at: Pos) = at.upRight
	override lazy val perpendicular = this :: Direction.computePerpendicular(this)
}

case object DownAndLeft extends Direction {
	override def apply(at: Pos) = at.downLeft
	override lazy val perpendicular = this :: Direction.computePerpendicular(this)
}

case object DownAndRight extends Direction {
	override def apply(at: Pos) = at.downRight
	override lazy val perpendicular = this :: Direction.computePerpendicular(this)
}

object Direction {

	def computePerpendicular(dir: Direction): List[Direction] = dir match {
		case UpAndLeft => List(UpAndRight, DownAndLeft)
		case UpAndRight => List(UpAndLeft, DownAndRight)
		case DownAndLeft => List(UpAndLeft, DownAndRight)
		case DownAndRight => List(UpAndLeft, DownAndRight)
	}

	val whitePawnMoves: List[Direction] = List(UpAndLeft, UpAndRight)
	val blackPawnMoves: List[Direction] = List(DownAndLeft, DownAndRight)

	val all: List[Direction] = List(UpAndLeft, UpAndRight, DownAndLeft, DownAndRight)

}


case class Move(piece: Piece, from: Pos, to: Pos, captureCount: Int, after: Board)


object Move {

	def whitePromoteFilter(pos: Pos): Boolean = (pos.m <= 5)
	def blackPromoteFilter(pos: Pos): Boolean = (pos.m >= 46)

	def legalMoves(prev: Board, side: Color): List[Move] = {

		def pawnMove(from: Pos, piece: Piece, dir: Direction): Option[Move] = {
			for (to <- dir(from) if prev(to).isEmpty; board <- prev.move(from, to))
				yield Move(piece, from, to, 0, board)
		}

		@tailrec
		def queenMove(from: Pos, pos: Pos, piece: Piece, dir: Direction, list: List[Move] = Nil): List[Move] = {
			val move = for (to <- dir(pos) if prev(to).isEmpty; board <- prev.move(from, to))
				yield Move(piece, from, to, 0, board)

			if (move.isEmpty) list
			else queenMove(from, move.get.to, piece, dir, move.get :: list)
		}

		def pawnCapture(from: Pos, pos: Pos, piece: Piece, currentBoard: Board, currentCaptures: Int): List[Move] = {
			def checkOpponent(dir: Direction): Option[Pos] = {
				for (to <- dir(pos); opponent <- prev(to) if opponent.is(!piece.color)) yield to
			}

			def checkEmpty(posOpponent: Pos, dir: Direction): Option[Pos] = {
				for (last <- dir(posOpponent) if prev(last).isEmpty) yield last
			}

			val moves = Direction.all flatMap { dir =>
				for (posOpponent <- checkOpponent(dir);
						posEmpty <- checkEmpty(posOpponent, dir);
						board <- currentBoard.take(pos, posEmpty, posOpponent))
					yield Move(piece, from, posEmpty, currentCaptures + 1, board)
			}

			val recMoves = moves flatMap { move => pawnCapture(from, move.to, piece, move.after, move.captureCount) }

			moves ++ recMoves
		}

		def queenCapture(from: Pos, pos: Pos, piece: Piece, currentBoard: Board, currentCaptures: Int, allowedDirs: List[Direction]): List[Move] = {
			@tailrec
			def firstOpponent(p: Pos, dir: Direction): List[Pos] = dir(p) match {
				case Some(to) => {
					if (prev(to).isEmpty) firstOpponent(to, dir)
					else if (prev(to).get.is(!piece.color)) List(to)
					else Nil
				}
				case _ => Nil
			}

			@tailrec
			def emptyPosAfter(p: Pos, dir: Direction, list: List[Pos] = Nil): List[Pos] = dir(p) match {
				case Some(to) if prev(to).isEmpty => emptyPosAfter(to, dir, to :: list)
				case _ => list
			}

			def buildMove(posOpponent: Pos, posEmpty: Pos): Option[Move] = {
				currentBoard.take(pos, posEmpty, posOpponent) map { board =>
					Move(piece, from, posEmpty, currentCaptures + 1, board)
				}
			}

			allowedDirs flatMap { dir =>
				val moves = { for (posOpponent <- firstOpponent(pos, dir);
									posEmpty <- emptyPosAfter(posOpponent, dir))
								yield buildMove(posOpponent, posEmpty)
							} flatten
				val recMoves = moves flatMap { move => queenCapture(from, move.to, piece, move.after, move.captureCount, dir.perpendicular) }
				moves ++ recMoves
			}
		}

		val promoteFilter: Pos => Boolean = if (side == White) whitePromoteFilter else blackPromoteFilter

		def promote(move: Move): Move = {
			if (!move.after.pieces.keys.exists(promoteFilter)) {
				System.err.println("aie")
				move
			} else {
				val newPieces = for ((pos, piece) <- move.after.pieces) yield {
					if (!promoteFilter(pos)) (pos, piece)
					else (pos, Piece(side, Queen))
				}
				System.err.println("YES")
				move.copy(after = Board(newPieces))
			}
		}

		def promoteAll(moves: List[Move]): List[Move] = moves map promote

		def dispatchCapture(pos: Pos, piece: Piece): List[Move] = piece.role match {
			case Pawn => promoteAll(pawnCapture(pos, pos, piece, prev, 0))
			case Queen => queenCapture(pos, pos, piece, prev, 0, Direction.all)
 		}

		def dispatchMove(pos: Pos, piece: Piece): List[Move] = piece match {
			case Piece(White, Pawn) => promoteAll(Direction.whitePawnMoves flatMap { dir => pawnMove(pos, piece, dir) })
			case Piece(Black, Pawn) => promoteAll(Direction.blackPawnMoves flatMap { dir => pawnMove(pos, piece, dir) })
			case Piece(_, Queen) => Direction.all flatMap { dir => queenMove(pos, pos, piece, dir) }
 		}

		val allCapture = prev.pieces flatMap { case (pos, piece) => if (piece is side) dispatchCapture(pos, piece) else Nil }

		if (allCapture.isEmpty) {
			val moves = prev.pieces flatMap { case (pos, piece) => if (piece is side) dispatchMove(pos, piece) else Nil }
			moves.toList

		} else {
			val maxCaptureCount = allCapture.maxBy { move => move.captureCount }.captureCount
			allCapture.filter(move => move.captureCount == maxCaptureCount).toList
		}
	}

}
