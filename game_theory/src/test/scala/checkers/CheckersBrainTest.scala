package checkers

import algo.HyperParameters
import algo.MinMax
import algo.AlphaBeta
import CheckersTest._


// clean
// set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
// compile


class CheckersBrainTest extends CheckersTest {

	val evalParams = EvalParameters(1.5, 4.0)

	def brainMM = new CheckersBrain(MinMax.findBestNode, HyperParameters(6), evalParams, true)
	def brainAB = new CheckersBrain(AlphaBeta.findBestNode, HyperParameters(8), evalParams, true)

	test("evaluation") {
		val eval = new Evaluation(evalParams)

		assert(eval.evaluate(new Moves(Board.init, White)) === 0, "Initial position should have 0 score")
		assert(eval.evaluate(new Moves(Board.init, Black)) === 0, "Initial position should have 0 score")

		val board1 = buildBoard(Map(Piece(White, Pawn) -> List(37, 28, 19)))
		assert(eval.evaluate(new Moves(board1, White)) > 0, "Score should be > 0 when won")
		assert(eval.evaluate(new Moves(board1, Black)) < 0, "Score should be < 0 when lost")

		val board2 = buildBoard(Map(Piece(White, Pawn) -> List(26),
									Piece(Black, Pawn) -> List(23, 19)))
		assert(eval.evaluate(new Moves(board2, White)) === -eval.evaluate(new Moves(board2, Black)),
			"White score should be opposite to Black score")

		val board3 = buildBoard(Map(Piece(White, Queen) -> List(32),
									Piece(Black, Pawn) -> List(19)))
		assert(eval.evaluate(new Moves(board3, White)) > 0, "Queen value should be higher than pawn value")

		val board4 = buildBoard(Map(Piece(White, Pawn) -> List(32, 33),
									Piece(Black, Pawn) -> List(25, 45)))
		assert(eval.evaluate(new Moves(board4, Black)) > 0, "Value of pawns at border should be higher than pawns in the middle")
	}

	test("simple strategy : white won") {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(22, 34, 35),
									Piece(Black, Pawn) -> List(18, 25)))
		val best = brainMM.bestMove(board, White)

		assert(!best._1.isEmpty)
		val move = best._1.get
		assert(move.piece === Piece(White, Pawn))
		assert((move.from.m, move.to.m, move.captureCount) === (22, 13, 1))

		val best2 = brainAB.bestMove(board, White)

		assert(!best2._1.isEmpty)
		val move2 = best2._1.get
		assert(move2.piece === Piece(White, Pawn))
		assert((move2.from.m, move2.to.m, move2.captureCount) === (22, 13, 1))

		val result = CheckersAutoPlayer.play(White, board, brainAB, brainAB)
		val winner = result._1
		assert(winner === Some(White))
	}

	test("simple strategy : black won") {
		val board = buildBoard(Map(Piece(White, Pawn) -> List(31, 34),
									Piece(Black, Queen) -> List(9),
									Piece(Black, Pawn) -> List(24)))
		val best = brainMM.bestMove(board, Black)

		assert(!best._1.isEmpty)
		val move = best._1.get
		assert(move.piece === Piece(Black, Queen))
		assert((move.from.m, move.to.m, move.captureCount) === (9, 36, 1))

		val best2 = brainAB.bestMove(board, Black)

		assert(!best2._1.isEmpty)
		val move2 = best2._1.get
		assert(move2.piece === Piece(Black, Queen))
		assert((move2.from.m, move2.to.m, move2.captureCount) === (9, 36, 1))

		val result = CheckersAutoPlayer.play(Black, board, brainAB, brainAB)
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
			val best = brainAB.bestMove(board, White)
			val eval = new Evaluation(brainAB.evalParams)

			assert(!best._1.isEmpty)
			val move = best._1.get
			assert((move.from.m, move.to.m, move.captureCount) === (26, 21, 0),
				"1st move is not the best one : " + move)
			assert(best._2 > eval.evaluate(new Moves(board, White)) * 100)

			val best2 = brainAB.bestMove(move.after, Black)

			assert(!best2._1.isEmpty)
			val move2 = best2._1.get
			assert((move2.from.m, move2.to.m, move2.captureCount) === (17, 37, 2),
				"2nd move is not the best one : " + move2)

			val best3 = brainAB.bestMove(move2.after, White)

			assert(!best3._1.isEmpty)
			val move3 = best3._1.get
			assert((move3.from.m, move3.to.m, move3.captureCount) === (28, 6, 2),
				"3rd move is not the best one : " + move3)
			assert(best3._2 > eval.evaluate(new Moves(move2.after, White)) * 100)

			val best4 = brainAB.bestMove(move3.after, Black)

			assert(!best4._1.isEmpty)
			val move4 = best4._1.get
			assert((move4.from.m, move4.to.m, move4.captureCount) === (37, 39, 2),
				"4th move is not the best one : " + move4)
		}
	}

	def logInfo(board: Board, side: Color, brain: CheckersBrain, best: (Option[Move], Double)) = {
		val move = best._1
		val score = best._2

		val bf = brain.branchingFactor.get
		val evalMin = brain.evaluationsSet.get.min * 100
		val evalMax = brain.evaluationsSet.get.max * 100
		val eval = new Evaluation(brain.evalParams)

		val mb = 1024*1024

		println("best move " + move.get.from.m + "x" + move.get.to.m)
		println("expanded " + brain.expandCount.get + " nodes with average branching factor of " + f"$bf%2.2f")
		println("initial score " + (eval.evaluate(new Moves(board, side)) * 100) + " up to " + (best._2 * 100))
		println(f"scores within values from $evalMin%2.2f to $evalMax%2.2f")
	}

	test("game start") {
		val brainW = brainAB
		val board = Board.init

		val best = brainW.bestMove(board, White)

		logInfo(board, White, brainW, best)

		assert(!best._1.isEmpty)
		assert(brainW.expandCount.get === 65253)
	}

	test("game middle") {
		val brain = brainAB
		val board = buildBoard(Map(Piece(White, Pawn) -> List(46, 47, 48, 49, 50, 41, 45, 36, 38, 39, 40, 33, 34, 35, 28),
									Piece(Black, Pawn) -> List(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 20, 26)))

		val best = brain.bestMove(board, Black)

		logInfo(board, Black, brain, best)

		assert(!best._1.isEmpty)
		assert(brain.expandCount.get === 201536)
	}

}
