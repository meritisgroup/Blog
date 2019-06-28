package checkers

import org.scalameter.api._
import CheckersTest._


class MovePerfNonReg extends Bench.OnlineRegressionReport {

	val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35),
								Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15),
								Piece(White, Queen) -> List(28),
								Piece(Black, Queen) -> List(20, 26)))

	performance of "NON REG Checkers" in {
		measure method "legalMoves" in {
			using(Gen.unit("board")) in {
				Unit => {
					(0 to 10).foreach { i =>
						new Moves(board, White).legalMoves
						new Moves(board, Black).legalMoves
					}
				}
			}
		}
	}

}
