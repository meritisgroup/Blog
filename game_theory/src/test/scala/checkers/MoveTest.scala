package checkers

import org.scalatest.FunSuite


class MoveTest extends FunSuite {

	test("pawn move") {
		val piece = Piece(Black, Pawn)
		val from = Pos.posAt(18).get

		val board1 = Board.empty.place(piece, from)
		val moves1 = Move.legalMoves(board1.get, Black)
		
		assert(moves1.size === 2)
		assert(moves1.map(m => m.piece).toSet === Set(piece))
		assert(moves1.map(m => m.from).toSet === Set(from))
		assert(moves1.map(m => m.to).toSet === Set(Pos.posAt(22).get, Pos.posAt(23).get))
		assert(moves1.map(m => m.captureCount).toSet === Set(0))
		assert(moves1.map(m => m.after.pieces.keys).flatten.toSet === Set(Pos.posAt(22).get, Pos.posAt(23).get))

		val board2 = Board.empty.place(Piece(White, Pawn), Pos.posAt(35).get)
		val moves2 = Move.legalMoves(board2.get, White)

		assert(moves2.size === 1)
		assert(moves2.map(m => m.to).toSet === Set(Pos.posAt(30).get))
		assert(moves2.map(m => m.after.pieces.keys).flatten.toSet === Set(Pos.posAt(30).get))
	}

}
