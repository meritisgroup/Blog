package checkers


// clean
// set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
// compile


class CheckersBrainTest extends CheckersTest {

	test("evaluation") {
		assert(Evaluation.evaluate(Board.init, White) === 0, "Initial position should have 0 score")
		assert(Evaluation.evaluate(Board.init, Black) === 0, "Initial position should have 0 score")

		val board1 = buildBoard(Map(Piece(White, Pawn) -> List(37, 28, 19)))
		assert(Evaluation.evaluate(board1, White) > 0, "Score should be > 0 when won")
		assert(Evaluation.evaluate(board1, Black) < 0, "Score should be < 0 when lost")

		val board2 = buildBoard(Map(Piece(White, Pawn) -> List(26),
									Piece(Black, Pawn) -> List(23, 19)))
		assert(Evaluation.evaluate(board2, White) === -Evaluation.evaluate(board2, Black), "White score should be opposite to Black score")

		val board3 = buildBoard(Map(Piece(White, Queen) -> List(32),
									Piece(Black, Pawn) -> List(19)))
		assert(Evaluation.evaluate(board3, White) > 0, "Queen value should be > than pawn value")

		val board4 = buildBoard(Map(Piece(White, Pawn) -> List(32, 33),
									Piece(Black, Pawn) -> List(25, 45)))
		assert(Evaluation.evaluate(board4, Black) > 0, "Value of pawns at border should be > than pawns in the middle")
	}

	def logInfo(brain: CheckersBrain, best: (Option[Move], Double)) = {
		val move = best._1
		val score = best._2

		assert(!move.isEmpty)
		System.out.print(move.get.from.m + " x " + move.get.to.m)
		System.out.print(" by evaluating " + brain.evaluationsCount.get + " nodes")
		System.out.println(" from scores within " + brain.evaluationsGetAll.get)
	}

	test("have fun") {
		val brain = new CheckersBrain(White, true)
		val best = brain.bestMove(Board.init)

		System.out.println("first white move:")
		logInfo(brain, best)

		val brain2 = new CheckersBrain(Black, true)
		val best2 = brain2.bestMove(best._1.get.after)

		System.out.println("reply from black:")
		logInfo(brain2, best2)
	}

}
