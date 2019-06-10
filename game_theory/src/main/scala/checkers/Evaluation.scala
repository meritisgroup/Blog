package checkers


object Evaluation {
	
	val pawnValue = 1.0
	val queenValue = 4.0
	val pawnSideWeight = 1.5

	def evaluate(moves: Moves): Double = {
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

		if (moves.win == Won) {
			1.0
		} else if (moves.win == Lost) {
			-1.0
		} else {
			val result = moves.current.pieces.foldLeft(0.0)((acc, elt) => {
				if (elt._2.color == moves.sideToPlay) acc + evaluateEach(elt._1, elt._2)
				else acc - evaluateEach(elt._1, elt._2)
			})
			result / 100.0
		}
	}

}
