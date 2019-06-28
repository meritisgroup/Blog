package checkers

import algo.HyperParameters
import algo.MinMax
import algo.AlphaBeta
import CheckersTest._


// clean
// set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
// compile


class CheckersBrainTest extends CheckersTest {

	def brainMM(side: Color) = new CheckersBrain(side, MinMax.findBestNode, HyperParameters(6), true)
	def brainAB(side: Color) = new CheckersBrain(side, AlphaBeta.findBestNode, HyperParameters(8), true)

	test("evaluation") {
		assert(Evaluation.evaluate(new Moves(Board.init, White)) === 0, "Initial position should have 0 score")
		assert(Evaluation.evaluate(new Moves(Board.init, Black)) === 0, "Initial position should have 0 score")

		val board1 = buildBoard(Map(Piece(White, Pawn) -> List(37, 28, 19)))
		assert(Evaluation.evaluate(new Moves(board1, White)) > 0, "Score should be > 0 when won")
		assert(Evaluation.evaluate(new Moves(board1, Black)) < 0, "Score should be < 0 when lost")

		val board2 = buildBoard(Map(Piece(White, Pawn) -> List(26),
									Piece(Black, Pawn) -> List(23, 19)))
		assert(Evaluation.evaluate(new Moves(board2, White)) === -Evaluation.evaluate(new Moves(board2, Black)),
			"White score should be opposite to Black score")

		val board3 = buildBoard(Map(Piece(White, Queen) -> List(32),
									Piece(Black, Pawn) -> List(19)))
		assert(Evaluation.evaluate(new Moves(board3, White)) > 0, "Queen value should be higher than pawn value")

		val board4 = buildBoard(Map(Piece(White, Pawn) -> List(32, 33),
									Piece(Black, Pawn) -> List(25, 45)))
		assert(Evaluation.evaluate(new Moves(board4, Black)) > 0, "Value of pawns at border should be higher than pawns in the middle")
	}

	test("simple strategy : white won") {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(22, 34, 35),
									Piece(Black, Pawn) -> List(18, 25)))
		val best = brainMM(White).bestMove(board)

		assert(!best._1.isEmpty)
		val move = best._1.get
		assert(move.piece === Piece(White, Pawn))
		assert((move.from.m, move.to.m, move.captureCount) === (22, 13, 1))

		val best2 = brainAB(White).bestMove(board)

		assert(!best2._1.isEmpty)
		val move2 = best2._1.get
		assert(move2.piece === Piece(White, Pawn))
		assert((move2.from.m, move2.to.m, move2.captureCount) === (22, 13, 1))

		val result = autoPlay(White, board, brainAB(White), brainAB(Black), false)
		val winner = result._1
		assert(winner === Some(White))
	}

	test("simple strategy : black won") {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(31, 34),
									Piece(Black, Queen) -> List(9),
									Piece(Black, Pawn) -> List(24)))
		val best = brainMM(Black).bestMove(board)

		assert(!best._1.isEmpty)
		val move = best._1.get
		assert(move.piece === Piece(Black, Queen))
		assert((move.from.m, move.to.m, move.captureCount) === (9, 36, 1))

		val best2 = brainAB(Black).bestMove(board)

		assert(!best2._1.isEmpty)
		val move2 = best2._1.get
		assert(move2.piece === Piece(Black, Queen))
		assert((move2.from.m, move2.to.m, move2.captureCount) === (9, 36, 1))

		val result = autoPlay(Black, board, brainAB(Black), brainAB(White), false)
		val winner = result._1
		assert(winner === Some(Black))
	}

	// See https://en.wikipedia.org/wiki/Draughts#General_rules
	trait ComplicatedStrategyBoard {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(26, 31, 32, 28, 33, 29, 38, 48, 40),
									Piece(Black, Pawn) -> List(7, 11, 17, 22, 13, 20, 24, 35, 41)))
	}

	test("complicated strategy : 4 first moves") {
		new ComplicatedStrategyBoard {
			val best = brainAB(White).bestMove(board)

			assert(!best._1.isEmpty)
			val move = best._1.get
			assert((move.from.m, move.to.m, move.captureCount) === (26, 21, 0),
				"1st move is not the best one : " + move)
			assert(best._2 > Evaluation.evaluate(new Moves(board, White)) * 100)

			val best2 = brainAB(Black).bestMove(move.after)

			assert(!best2._1.isEmpty)
			val move2 = best2._1.get
			assert((move2.from.m, move2.to.m, move2.captureCount) === (17, 37, 2),
				"2nd move is not the best one : " + move2)

			val best3 = brainAB(White).bestMove(move2.after)

			assert(!best3._1.isEmpty)
			val move3 = best3._1.get
			assert((move3.from.m, move3.to.m, move3.captureCount) === (28, 6, 2),
				"3rd move is not the best one : " + move3)
			assert(best3._2 > Evaluation.evaluate(new Moves(move2.after, White)) * 100)

			val best4 = brainAB(Black).bestMove(move3.after)

			assert(!best4._1.isEmpty)
			val move4 = best4._1.get
			assert((move4.from.m, move4.to.m, move4.captureCount) === (37, 39, 2),
				"4th move is not the best one : " + move4)
		}
	}

	def checkMoves(moves: List[Move], refs: List[(Int, Int, Int)]) = {
		for (i <- 0 until refs.size) {
			assert(moves.size > i, "missing move at step " + (i+1))
			assert((moves(i).from.m, moves(i).to.m, moves(i).captureCount) === (refs(i)._1, refs(i)._2, refs(i)._3),
				"Move at step " + (i+1) + " is not the best one")
		}
		assert(moves.size === refs.size)
	}

	test("complicated strategy : all moves until the end") {
		new ComplicatedStrategyBoard {
			val player1 = brainAB(White)
			val player2 = brainAB(Black)

			val result = autoPlay(White, board, player1, player2, false)
			val winner = result._1
			val moves = result._2

			checkMoves(moves, List((26, 21, 0), (17, 37, 2),
									(28, 6, 2), (37, 39, 2),
									(6, 1, 0), (24, 42, 2),
									(6, 43, 4), (35, 44, 0)))

			assert(winner.isDefined)
			assert(winner.get === White)
		}
	}

	def logInfo(board: Board, brain: CheckersBrain, best: (Option[Move], Double)) = {
		val move = best._1
		val score = best._2

		val bf = brain.branchingFactor.get
		val evalMin = brain.evaluationsSet.get.min * 100
		val evalMax = brain.evaluationsSet.get.max * 100

		val mb = 1024*1024

		println("best move " + move.get.from.m + "x" + move.get.to.m)
		println("expanded " + brain.expandCount.get + " nodes with average branching factor of " + f"$bf%2.2f")
		println("initial score " + (Evaluation.evaluate(new Moves(board, brain.side)) * 100) + " up to " + (best._2 * 100))
		println(f"scores within values from $evalMin%2.2f to $evalMax%2.2f")
	}

	test("game start") {
		val brainW = brainAB(White)
		val board = Board.init

		val best = brainW.bestMove(board)

		logInfo(board, brainW, best)

		assert(!best._1.isEmpty)
		assert(brainW.expandCount.get === 65253)
	}

	test("game middle") {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35, 28),
									Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 20, 26)))

		val brain = brainAB(Black)

		val best = brain.bestMove(board)

		logInfo(board, brain, best)

		assert(!best._1.isEmpty)
		assert(brain.expandCount.get === 201536)
	}

}
