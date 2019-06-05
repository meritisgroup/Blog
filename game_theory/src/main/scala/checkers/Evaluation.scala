package checkers


object Evaluation {
	
	val pawnValue = 1.0
	val queenValue = 4.0
	val pawnSideWeight = 1.5

	def evaluate(board: Board, side: Color): Double = {
		def pawnWeight(pos: Pos): Double = {
			if (pos.m >= 41
					|| pos.m <= 10
					|| pos.m == 16 || pos.m == 26 || pos.m == 36
					|| pos.m == 15 || pos.m == 25 || pos.m == 35) {
				pawnSideWeight
			} else 1.0
		}

		def evaluateEach(pos: Pos, piece: Piece): Double = piece.role match {
			case Pawn => pawnValue * pawnWeight(pos)
			case Queen => queenValue
		}

		val gameStatus = Move.win(board, side)

		if (gameStatus == Won) {
			Double.MaxValue
		} else if (gameStatus == Lost) {
			Double.MinValue
		} else {
			board.pieces.foldLeft(0.0)((acc, elt) => {
				if (elt._2.color == side) acc + evaluateEach(elt._1, elt._2)
				else acc - evaluateEach(elt._1, elt._2)
			})
		}
	}

}
