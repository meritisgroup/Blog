package chess

import org.scalatest.FunSuite
import Pos._


class MoveTest extends FunSuite {
	
	test("data referential") {
		assert(White.white)
		assert(Black.black)

		Pos.all.foreach { pos =>
			assert(pos != null)
			assert(pos.down != null)
			assert(pos.up != null)
			assert(pos.left != null)
			assert(pos.right != null)
			assert(pos.downLeft != null)
			assert(pos.downRight != null)
			assert(pos.upLeft != null)
			assert(pos.upRight != null)
		}

		assert(Piece(White, Queen).hashCode != Piece(Black, Pawn).hashCode)
	}

	def fromEmpty(piece: Piece, origin: Pos): Board = {
		Board.empty.place(piece, origin).get
	}

	def fromInit(list: List[(Pos, Pos)]): Board = {
		def rec(board: Board, elt: (Pos,Pos)): Board = {
			if (board.contains(elt._2)) board.take(elt._1, elt._2).get
			else board.move(elt._1, elt._2).get
		}

		list.foldLeft(Board.init)(rec)
	}

	def count(board: Board, color: Color): Int = {
		board.pieces.values.filter(_.is(color)).size
	}

	def checkMove(board: Board, origin: Pos, refs: Set[Pos]) {
		val piece = board(origin).get
		val list = new Moves(board, History(), piece.color).legalMoves.filter(_.origin == origin)

		assert(list.map(_.piece).toSet === Set(piece))
		assert(list.map(_.dest).toSet === refs)
	}

	def checkTake(before: Board, piece: Piece, origin: Pos, dest: Pos) {
		val moves = new Moves(before, History(), piece.color).legalMoves
		val list = moves.filter(move => move.origin == origin && move.dest == dest)

		assert(list.size === 1)

		val after = list.head.after

		assert(after(origin).isEmpty)
		assert(!after(dest).isEmpty)
		assert(after(dest).get === piece)

		assert(count(before, piece.color) === count(after, piece.color))
		assert(count(before, !piece.color) === count(after, !piece.color) + 1)
	}

	test("knight move") {
		val piece = Piece(White, Knight)

		val board1 = fromEmpty(piece, C3)
		checkMove(board1, C3, Set(D1, E2, E4, D5, B5, A4, A2, B1))

		val board2 = Board.init
		checkMove(board2, B1, Set(A3, C3))

		val board3 = board2.move(A2, A3).get
		checkMove(board3, B1, Set(C3))
	}

	test("knight capture") {
		val piece = Piece(White, Knight)

		val board = fromInit(List((B7, B5), (B1, A3), (H7, H6)))
		checkTake(board, piece, A3, B5)
	}

	test("king move") {
		val piece = Piece(White, King)

		val board1 = fromEmpty(piece, C3)
		checkMove(board1, C3, Set(C2, D2, D3, D4, C4, B4, B3, B2))

		val board2 = Board.init.move(E2, E4).get
		checkMove(board2, E1, Set(E2))

		val board3 = board2.move(D2, D3).get
		checkMove(board3, E1, Set(D2, E2))
	}

	test("king capture") {
		val piece = Piece(White, King)

		val board = fromInit(List((E7, E2)))
		checkTake(board, piece, E1, E2)
	}

	test("bishop move") {
		val piece = Piece(Black, Bishop)

		val board1 = fromEmpty(piece, D4)
		checkMove(board1, D4, Set(C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))

		val board2 = Board.init.move(G7, G6).get
		checkMove(board2, F8, Set(G7, H6))

		val board3 = board2.move(E7, E6).get
		checkMove(board3, F8, Set(G7, H6, E7, D6, C5, B4, A3))
	}

	test("bishop capture") {
		val piece = Piece(Black, Bishop)

		val board = fromInit(List((E7, E6), (A2, A3)))
		checkTake(board, piece, F8, A3)
	}

	test("rook move") {
		val piece = Piece(Black, Rook)

		val board1 = fromEmpty(piece, D4)
		checkMove(board1, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8))

		val board2 = Board.init.move(A7, A5).get
		checkMove(board2, A8, Set(A7, A6))

		val board3 = board2.move(A8, A6).get
		checkMove(board3, A6, Set(A7, A8, B6, C6, D6, E6, F6, G6, H6))
	}

	test("rook capture") {
		val piece = Piece(Black, Rook)

		val board = fromInit(List((B2, B4), (A7, A5), (B4, A5)))
		checkTake(board, piece, A8, A5)
	}

	test("queen move") {
		val piece = Piece(White, Queen)

		val board1 = fromEmpty(piece, D4)
		checkMove(board1, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8,
								C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))

		val board2 = fromEmpty(piece, H8)
		checkMove(board2, H8, Set(H7, H6, H5, H4, H3, H2, H1, A8, B8, C8, D8, E8, F8, G8,
								A1, B2, C3, D4, E5, F6, G7))

		val board3 = Board.init.move(C2, C4).get
		checkMove(board3, D1, Set(C2, B3, A4))

		val board4 = board3.move(D2, D4).get
		checkMove(board4, D1, Set(C2, B3, A4, D2, D3))
	}

	test("queen capture") {
		val piece = Piece(White, Queen)

		val board = fromInit(List((C2, C3), (G7, G6), (D1, B3), (A7, A6)))
		checkTake(board, piece, B3, F7)
	}

	test("pawn move") {
		val piece = Piece(White, Pawn)

		val board1 = fromEmpty(piece, D2)
		checkMove(board1, D2, Set(D3, D4))

		val board2 = fromEmpty(piece, D3)
		checkMove(board2, D3, Set(D4))
	}

	test("pawn capture") {
		val piece = Piece(Black, Pawn)

		val board = fromInit(List((E2, E4), (D7, D5)))
		checkTake(board, piece, D5, E4)
	}

	test("pawn promotion") {
		val piece = Piece(White, Pawn)

		val board = fromEmpty(piece, D7)

		val list = new Moves(board, History(), piece.color).legalMoves

		assert(list.size === 1)

		val move = list.head

		assert(move.piece === piece)
		assert(move.origin === D7)
		assert(move.dest === D8)
		assert(move.after(D8) === Some(Piece(White, Queen)))
	}

	def shortCastle(y: Int, color: Color) {
		val piece = Piece(color, King)
		val board = fromInit(List((G2, G3), (G7, G6), (G1, F3), (G8, F6), (F1, H3), (F8, H6)))
		val list = new Moves(board, History(), color).legalMoves.filter(_.origin == Pos.posAt(5, y).get)

		assert(list.size === 2)
		assert(list.map(_.piece).toSet === Set(piece))
		assert(list.map(_.dest).toSet === Set(Pos.posAt(6, y).get, Pos.posAt(7, y).get))

		val move = list.find(move => move.dest == Pos.posAt(7, y).get).get

		assert(!move.after.contains(Pos.posAt(5, y).get))
		assert(!move.after.contains(Pos.posAt(8, y).get))
		assert(move.after(Pos.posAt(6, y).get) === Some(Piece(color, Rook)))
		assert(move.after(Pos.posAt(7, y).get) === Some(Piece(color, King)))

		val history = move.afterHistory

		assert(history.allowed(color, true) === false)
		assert(history.allowed(color, false) === false)
	}

	test("short castle") {
		shortCastle(1, White)
		shortCastle(8, Black)
	}

	def longCastle(y: Int, color: Color) {
		val piece = Piece(color, King)
		val yBis = if (y == 1) 2 else 7
		val board = fromInit(List((B2, B3), (B7, B6), (B1, C3), (B8, C6), (C1, A3), (C8, A6),
									(E2, E3), (E7, E6), (D1, F3), (D8, F6)))
		val list = new Moves(board, History(), color).legalMoves.filter(_.origin == Pos.posAt(5, y).get)

		assert(list.size === 2)
		assert(list.map(_.piece).toSet === Set(piece))
		assert(list.map(_.dest).toSet === Set(Pos.posAt(4, y).get, Pos.posAt(3, y).get))

		val move = list.find(move => move.dest == Pos.posAt(3, y).get).get

		assert(!move.after.contains(Pos.posAt(5, y).get))
		assert(!move.after.contains(Pos.posAt(1, y).get))
		assert(!move.after.contains(Pos.posAt(2, y).get))
		assert(move.after(Pos.posAt(4, y).get) === Some(Piece(color, Rook)))
		assert(move.after(Pos.posAt(3, y).get) === Some(Piece(color, King)))

		val history = move.afterHistory

		assert(history.allowed(color, true) === false)
		assert(history.allowed(color, false) === false)
	}

	test("long castle") {
		longCastle(1, White)
		longCastle(8, Black)
	}

	test("prise en passant - black side") {
		val board = fromInit(List((G2, G3), (D7, D4), (H7, H4)))
		val moves = new Moves(board, History(), White).legalMoves

		val move1 = moves.filter(move => move.origin == E2 && move.dest == E4).head
		val all1 = new Moves(move1.after, move1.afterHistory, Black).legalMoves
		val list1 = all1.filter(move => move.origin == D4 && move.dest == E3)

		assert(list1.size === 1)
		assert(list1.head.piece === Piece(Black, Pawn))
		assert(list1.head.origin === D4)

		val after = list1.head.after

		assert(!after.contains(E4))
		assert(count(move1.after, Black) === count(after, Black))
		assert(count(move1.after, White) === count(after, White) + 1)

		val move2 = moves.filter(move => move.origin == G3 && move.dest == G4).head
		val all2 = new Moves(move2.after, move2.afterHistory, Black).legalMoves
		val list2 = all2.filter(move => move.origin == H4 && move.dest == G3)

		assert(list2.isEmpty)
	}

	test("prise en passant - white side") {
		val board = fromInit(List((G7, G6), (D2, D5), (H2, H5)))
		val moves = new Moves(board, History(), Black).legalMoves

		val move1 = moves.filter(move => move.origin == E7 && move.dest == E5).head
		val all1 = new Moves(move1.after, move1.afterHistory, Black).legalMoves
		val list1 = all1.filter(move => move.origin == D5 && move.dest == E6)

		assert(list1.size === 1)
		assert(list1.head.piece === Piece(White, Pawn))
		assert(list1.head.origin === D5)

		val after = list1.head.after

		assert(!after.contains(E5))
		assert(count(move1.after, Black) === count(after, Black) + 1)
		assert(count(move1.after, White) === count(after, White))

		val move2 = moves.filter(move => move.origin == G6 && move.dest == G5).head
		val all2 = new Moves(move2.after, move2.afterHistory, White).legalMoves
		val list2 = all2.filter(move => move.origin == H5 && move.dest == G6)

		assert(list2.isEmpty)
	}

}
