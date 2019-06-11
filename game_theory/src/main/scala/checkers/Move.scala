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
	lazy val legalMoves: List[Move] = computeMoves

	def computeStatus: Status = {
		val count = current.pieces.foldLeft((0, 0))((acc, elt) => {
			if (elt._2.color == sideToPlay) (acc._1 + 1, acc._2) else (acc._1, acc._2 + 1)
		})

		if (count._1 == 0) Lost
		else if (count._2 == 0) Won
		else if (legalMoves.isEmpty) Lost
		else Ongoing
	}

	def pawnMove(from: Pos, piece: Piece, dir: Direction): Option[Move] = {
		for (to <- dir(from) if current(to).isEmpty; board <- current.move(from, to))
			yield Move(piece, from, to, 0, board)
	}

	@tailrec
	final def queenMove(from: Pos, pos: Pos, piece: Piece, dir: Direction, list: List[Move] = Nil): List[Move] = {
		val move = for (to <- dir(pos) if current(to).isEmpty; board <- current.move(from, to))
			yield Move(piece, from, to, 0, board)

		if (move.isEmpty) list
		else queenMove(from, move.get.to, piece, dir, move.get :: list)
	}

	def pawnCapture(from: Pos, pos: Pos, piece: Piece, currentBoard: Board, currentCaptures: Int): List[Move] = {
		def checkOpponent(dir: Direction): Option[Pos] = {
			for (to <- dir(pos); opponent <- current(to) if opponent.is(!piece.color)) yield to
		}

		def checkEmpty(posOpponent: Pos, dir: Direction): Option[Pos] = {
			for (last <- dir(posOpponent) if current(last).isEmpty) yield last
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
				if (current(to).isEmpty) firstOpponent(to, dir)
				else if (current(to).get.is(!piece.color)) List(to)
				else Nil
			}
			case _ => Nil
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
			val moves = { for (posOpponent <- firstOpponent(pos, dir);
								posEmpty <- emptyPosAfter(posOpponent, dir))
							yield buildMove(posOpponent, posEmpty)
						}.flatten
			val recMoves = moves flatMap { move => queenCapture(from, move.to, piece, move.after, move.captureCount, dir.perpendiculars) }
			moves ++ recMoves
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

	/*def promote(move: Move): Move = {
		if (!move.after.pieces.keys.exists(promoteFilter)) {
			move
		} else {
			val newPieces = for ((pos, piece) <- move.after.pieces) yield {
				if (!promoteFilter(pos)) (pos, piece)
				else (pos, Piece(sideToPlay, Queen))
			}
			move.copy(after = Board(newPieces, 0))
		}
	}*/

	def promoteAll(moves: List[Move]): List[Move] = moves map promote

	def computeMoves: List[Move] = {

		def dispatchCapture(pos: Pos, piece: Piece): List[Move] = piece.role match {
			case Pawn => promoteAll(pawnCapture(pos, pos, piece, current, 0))
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
