package checkers

import algo.HyperParameters
import algo.MinMax
import algo.AlphaBeta


// clean
// set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
// compile


class CheckersBrainTest extends CheckersTest {

	def brainMM(side: Color) = new CheckersBrain(side, MinMax.findBestNode, HyperParameters(6))
	def brainAB(side: Color) = new CheckersBrain(side, AlphaBeta.findBestNode, HyperParameters(10))

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
				"Move at step " + (i+1) + " is not the best one : " + moves(i))
		}
		assert(moves.size === refs.size)
	}

	// Depth should be 10 and then, it is too slow...
	// At depth 8, algo does not find the best move
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

	def logInfo(brain: CheckersBrain, best: (Option[Move], Double), elapsedTime: Long) = {
		val move = best._1
		val score = best._2

		println(move.get.from.m + " x " + move.get.to.m)
		println("by expanding " + brain.expandCount.get + " nodes with computation time " + (elapsedTime) + " msec")
		println("from scores within " + brain.evaluationsSet.get)
	}

	test("game start - 1st white move") {
		val brainW = brainAB(White)
		val startTime = System.currentTimeMillis()
		val best = brainW.bestMove(Board.init)
		val elapsedTime = System.currentTimeMillis() - startTime

		logInfo(brainW, best, elapsedTime)

		assert(!best._1.isEmpty)
		assert(brainW.expandCount.get === 554224)
	}

	test("game start - 2nd black move") {
		val brainB = brainAB(Black)

		val startTime = System.currentTimeMillis()
		val best = brainB.bestMove(Board.init.move(Pos.posAt(35).get, Pos.posAt(30).get).get)
		val elapsedTime = System.currentTimeMillis() - startTime

		logInfo(brainB, best, elapsedTime)

		assert(!best._1.isEmpty)
		assert(brainB.expandCount.get === 541684)
	}

}
