package checkers


case class Move(piece: Piece, from: Pos, to: Pos, captureCount: Int, after: Board)


object Move {

	val whitePawnDirs: List[Pos => Option[Pos]] = List(p => p.upLeft, p => p.upRight)
	val blackPawnDirs: List[Pos => Option[Pos]] = List(p => p.downLeft, p => p.downRight)

	val allDirs: List[Pos => Option[Pos]] = List(p => p.upLeft, p => p.upRight, p => p.downLeft, p => p.downRight)

	def legalMoves(prev: Board, side: Color): List[Move] = {

		def pawnMove(pos: Pos, piece: Piece, dirs: List[Pos => Option[Pos]]): List[Move] = {
			dirs flatMap { dir => dir(pos) } flatMap { p =>
				if (!prev(p).isEmpty) Nil
				else prev.move(pos, p) map { board => Move(piece, pos, p, 0, board) }
			}
		}

		def pawnCapture(pos: Pos, piece: Piece): List[Move] = {
			allDirs flatMap { dir =>
				val posEnemy = dir(pos)
				if (!posEnemy.isEmpty && !prev(posEnemy.get).isEmpty && prev(posEnemy.get).get.is(!piece.color)) {
					val posEmpty = dir(posEnemy.get)
					if (!posEmpty.isEmpty && prev(posEmpty.get).isEmpty) {
						prev.take(pos, posEmpty.get, posEnemy.get) map { board => Move(piece, pos, posEmpty.get, 1, board) }
					} else Nil
				} else Nil
			}
		}

		val all = for ((pos, piece) <- prev.pieces) yield {
			piece match {
				case Piece(White, Pawn) => pawnMove(pos, piece, whitePawnDirs) ++ pawnCapture(pos, piece)
				case Piece(Black, Pawn) => pawnMove(pos, piece, blackPawnDirs) ++ pawnCapture(pos, piece)
				case Piece(_, Queen) => Nil
			}
		}

		all.toList.flatten
	}

}
