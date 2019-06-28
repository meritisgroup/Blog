package checkers

import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import algo.HyperParameters
import algo.MinMax
import algo.AlphaBeta
import CheckersTest._


// sbt command : testOnly *Benchmark*
class CheckersBenchmark extends Bench.ForkedTime {

	val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35),
								Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15),
								Piece(White, Queen) -> List(28),
								Piece(Black, Queen) -> List(20, 26)))

	performance of "PERF TEST Checkers" in {
		measure method "Best move with alpha beta and depth 6" in {
			using(Gen.unit("board")) in {
				Unit => new CheckersBrain(Black, AlphaBeta.findBestNode, HyperParameters(6)).bestMove(board)
			}
		}
	}

	performance of "PERF TEST Checkers" in {
		measure method "Pawn & queen capture" in {
			using(Gen.unit("board")) in {
				Unit => {
					val moves = new Moves(board, White)
					val moves2 = new Moves(board, Black)
					(0 to 10).foreach { i =>
						board.pieces flatMap { case (pos, piece) =>
							if ((piece is White) && (piece is Pawn)) {
								//Direction.whitePawnMoves flatMap { dir => moves.pawnMove(pos, piece, dir) }
								Direction.all flatMap { dir => moves.pawnCapture(pos, pos, piece, board, 0, dir) }
							} else if ((piece is Black) && (piece is Pawn)) {
								//Direction.blackPawnMoves flatMap { dir => moves2.pawnMove(pos, piece, dir) }
								Direction.all flatMap { dir => moves2.pawnCapture(pos, pos, piece, board, 0, dir) }
							} else if ((piece is White) && (piece is Queen)) {
								//Direction.all flatMap { dir => moves.queenMove(pos, pos, piece, dir) }
								moves.queenCapture(pos, pos, piece, board, 0, Direction.all)
							} else if ((piece is Black) && (piece is Queen)) {
								//Direction.all flatMap { dir => moves2.queenMove(pos, pos, piece, dir) }
								moves2.queenCapture(pos, pos, piece, board, 0, Direction.all)
							} else Nil
						}
					}
				}
			}
		}
	}

}
