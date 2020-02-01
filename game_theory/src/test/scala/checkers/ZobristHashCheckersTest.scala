package checkers


class ZobristHashCheckersTest extends CheckersTest {
	
	test("hash with Board.place") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.place(Piece(White, Pawn), Pos.posAt(28).get).get

		assert(nextBoard.hash === ZobristHashCheckers.computeHash(nextBoard.pieces))
	}

	test("hash with Board.replace") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.replace(Piece(White, Queen), Pos.posAt(31).get).get

		assert(nextBoard.hash === ZobristHashCheckers.computeHash(nextBoard.pieces))
	}

	test("hash with Board.remove") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.remove(Pos.posAt(13).get).get

		assert(nextBoard.hash === ZobristHashCheckers.computeHash(nextBoard.pieces))
	}

	test("hash with Board.move") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.move(Pos.posAt(32).get, Pos.posAt(28).get).get

		assert(nextBoard.hash === ZobristHashCheckers.computeHash(nextBoard.pieces))
	}

	test("hash with Board.take") {
		val firstBoard = Board.init
		val firstBoard1 = firstBoard.move(Pos.posAt(17).get, Pos.posAt(22).get).get
		val firstBoard2 = firstBoard1.move(Pos.posAt(22).get, Pos.posAt(28).get).get

		val nextBoard = firstBoard2.take(Pos.posAt(33).get, Pos.posAt(22).get, Pos.posAt(28).get).get

		assert(nextBoard.hash === ZobristHashCheckers.computeHash(nextBoard.pieces))
	}

}
