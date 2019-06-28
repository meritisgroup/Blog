package checkers

import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import algo.HyperParameters
import algo.MinMax
import algo.AlphaBeta
import CheckersTest._


// sbt command : testOnly *Benchmark*
class CheckersBenchmark extends Bench.ForkedTime {

	/*val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35),
								Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15),
								Piece(White, Queen) -> List(28),
								Piece(Black, Queen) -> List(20, 26)))*/

	val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35, 28),
								Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 20, 26)))

	performance of "PERF TEST Checkers" in {
		measure method "Best move from Alpha beta" in {
			using(Gen.unit("board")) in {
				Unit => new CheckersBrain(Black, AlphaBeta.findBestNode, HyperParameters(6)).bestMove(board)
			}
		}
	}

}
