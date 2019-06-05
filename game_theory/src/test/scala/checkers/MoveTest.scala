package checkers


class MoveTest extends CheckersTest {

	def checkTo(side: Color, from: Int, to: List[Int], moves: List[Move]) = {
		val set = to.map(x => Pos.posAt(x).get).toSet

		assert(moves.map(move => move.from.m).toSet === Set(from))
		assert(moves.map(move => move.to).toSet === set)
		assert(moves.map(move => move.after.pieces.filter(_._2 is side).keys).flatten.toSet === set)
	}

	test("pawn move") {
		val piece = Piece(Black, Pawn)
		val fromBlack = 18
		val fromWhite = 35

		val board = buildBoard(Map(piece -> List(fromBlack),
									Piece(White, Pawn) -> List(fromWhite)))
		val moves1 = Move.legalMoves(board, Black)
		
		assert(moves1.size === 2)
		assert(moves1.map(move => move.piece).toSet === Set(piece))
		assert(moves1.map(move => move.captureCount).toSet === Set(0))
		checkTo(Black, fromBlack, List(22, 23), moves1)

		val moves2 = Move.legalMoves(board, White)

		assert(moves2.size === 1)
		checkTo(White, fromWhite, List(30), moves2)
	}

	test("queen move") {
		val piece = Piece(Black, Queen)
		val from = 18

		val board = buildBoard(Map(piece -> List(from)))
		val moves = Move.legalMoves(board, Black)

		assert(moves.size === 15)
		assert(moves.map(move => move.piece).toSet === Set(piece))
		assert(moves.map(move => move.captureCount).toSet === Set(0))
		checkTo(Black, from, List(1, 7, 12, 13, 9, 4, 22, 27, 31, 36, 23, 29, 34, 40, 45), moves)
	}

	test("pawn capture x1") {
		val piece = Piece(Black, Pawn)
		val from = 18

		val board = buildBoard(Map(piece -> List(from),
									Piece(White, Pawn) -> List(22, 13)))
		val moves = Move.legalMoves(board, Black).filter(m => m.captureCount > 0)

		assert(moves.size === 2)
		assert(moves.map(move => move.piece).toSet === Set(piece))
		assert(moves.map(move => move.captureCount).toSet === Set(1))
		checkTo(Black, from, List(9, 27), moves)
		assert(moves.find(move => move.to.m == 9).get.after(Pos.posAt(13).get).isEmpty)
		assert(moves.find(move => move.to.m == 27).get.after(Pos.posAt(22).get).isEmpty)
	}

	test("pawn capture x2") {
		val piece = Piece(Black, Pawn)
		val from = 18

		val board = buildBoard(Map(piece -> List(from),
									Piece(White, Pawn) -> List(22, 13, 14)))
		val moves = Move.legalMoves(board, Black).filter(m => m.captureCount > 0)

		assert(moves.size === 1)
		assert(moves.map(move => move.piece).toSet === Set(piece))
		assert(moves.map(move => move.captureCount).toSet === Set(2))
		checkTo(Black, from, List(20), moves)
		assert(moves.head.after(Pos.posAt(13).get).isEmpty)
		assert(moves.head.after(Pos.posAt(14).get).isEmpty)
		assert(!moves.head.after(Pos.posAt(22).get).isEmpty)
	}

	test("queen capture") {
		val piece = Piece(White, Queen)
		val from = 28

		val board = buildBoard(Map(piece -> List(from),
									Piece(Black, Pawn) -> List(32, 44)))
		val moves = Move.legalMoves(board, White).filter(m => m.captureCount > 0)

		assert(moves.size === 4)
		assert(moves.map(move => move.piece).toSet === Set(piece))
		assert(moves.map(move => move.captureCount).toSet === Set(1))
		checkTo(White, from, List(37, 41, 46, 50), moves)

		assert(moves.find(move => Set(37, 41, 46) contains move.to.m).get.after(Pos.posAt(32).get).isEmpty)
		assert(!moves.find(move => Set(37, 41, 46) contains move.to.m).get.after(Pos.posAt(44).get).isEmpty)

		assert(moves.find(move => move.to.m == 50).get.after(Pos.posAt(44).get).isEmpty)
		assert(!moves.find(move => move.to.m == 50).get.after(Pos.posAt(32).get).isEmpty)
	}

	test("queen capture x4") {
		val piece = Piece(White, Queen)
		val from = 44

		val board = buildBoard(Map(piece -> List(from),
									Piece(Black, Pawn) -> List(39, 28, 7, 30)))
		val moves = Move.legalMoves(board, White).filter(m => m.captureCount > 0)

		assert(moves.size === 1)
		assert(moves.map(move => move.captureCount).toSet === Set(4))
		checkTo(White, from, List(35), moves)
		assert(moves.head.after.pieces.size === 1)
	}

	test("queen capture - turk move") {
		val piece = Piece(Black, Queen)
		val from = 35

		val board = buildBoard(Map(piece -> List(from),
									Piece(White, Pawn) -> List(30, 29, 23, 38, 39)))
		val moves = Move.legalMoves(board, Black).filter(m => m.captureCount > 0)

		assert(moves.size === 1)
		assert(moves.map(move => move.captureCount).toSet === Set(4))
		checkTo(White, from, List(34), moves)
		assert(moves.head.after(Pos.posAt(29).get) === Piece(White, Pawn))
	}

	test("pawn promotion") {
		val board1 = buildBoard(Map(Piece(White, Pawn) -> List(7)))
		val moves1 = Move.legalMoves(board1, White)

		assert(moves1.size === 2)
		assert(moves1.flatMap(move => move.after.pieces.values).toSet === Set(Piece(White, Queen)))
		checkTo(White, 7, List(1, 2), moves1)

		val board2 = buildBoard(Map(Piece(White, Pawn) -> List(12),
									Piece(Black, Pawn) -> List(7)))
		val moves2 = Move.legalMoves(board2, White).filter(m => m.captureCount > 0)

		assert(moves2.size === 1)
		assert(moves2.flatMap(move => move.after.pieces.values).toSet === Set(Piece(White, Queen)))
		checkTo(White, 12, List(1), moves2)

		val board3 = buildBoard(Map(Piece(White, Pawn) -> List(11),
									Piece(Black, Pawn) -> List(7, 8)))
		val moves3 = Move.legalMoves(board3, White).filter(m => m.captureCount > 0)

		assert(moves3.size === 1)
		assert(moves3.flatMap(move => move.after.pieces.values).toSet === Set(Piece(White, Pawn)))
		checkTo(White, 11, List(13), moves3)
	}

	test("win") {
		assert(Move.win(Board.init, White) === Ongoing)
		assert(Move.win(Board.init, Black) === Ongoing)

		// Only white pawns
		val board1 = buildBoard(Map(Piece(White, Pawn) -> List(37, 28, 19)))
		assert(Move.win(board1, White) === Won)
		assert(Move.win(board1, Black) === Lost)

		// White pawns blocked
		val board2 = buildBoard(Map(Piece(White, Pawn) -> List(26),
									Piece(Black, Pawn) -> List(21, 17)))
		assert(Move.win(board2, White) === Lost)
		assert(Move.win(board2, Black) === Ongoing)
	}

}
