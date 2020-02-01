package checkers


class PieceTest extends CheckersTest {

	def checkRowCol(m: Int, row: Int, col: Int) = {
		assert(Pos.row(m) === row)
		assert(Pos.col(m) === col)
		assert(Pos.manoury(row, col) === Some(m))
	}

	test("pos : manoury conversion") {
		checkRowCol(1, 0, 1)
		checkRowCol(5, 0, 9)
		checkRowCol(6, 1, 0)
		checkRowCol(10, 1, 8)
		checkRowCol(23, 4, 5)
		checkRowCol(45, 8, 9)
		checkRowCol(50, 9, 8)

		assert(Pos.manoury(-1, 5) === None)
		assert(Pos.manoury(0, 0) === None)
		assert(Pos.manoury(5, 1) === None)
	}

	def checkNeighbors(m: Int, dl: Option[Pos], dr: Option[Pos], ur: Option[Pos], ul: Option[Pos]) = {
		assert(Pos.posAt(m).get.downLeft === dl)
		assert(Pos.posAt(m).get.downRight === dr)
		assert(Pos.posAt(m).get.upRight === ur)
		assert(Pos.posAt(m).get.upLeft === ul)
	}

	test("pos down/up left/right neighbors") {
		checkNeighbors(1, Pos.posAt(6), Pos.posAt(7), None, None)
		checkNeighbors(5, Pos.posAt(10), None, None, None)
		checkNeighbors(46, None, None, Pos.posAt(41), None)
		checkNeighbors(50, None, None, Pos.posAt(45), Pos.posAt(44))
		checkNeighbors(23, Pos.posAt(28), Pos.posAt(29), Pos.posAt(19), Pos.posAt(18))
	}

}
