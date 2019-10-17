package chess

import org.scalatest.FunSuite
import Pos._


class ActorTest extends FunSuite {
	
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

	def checkMove(role: Role, init: Pos, refs: Set[Pos]) {
		val piece = Piece(White, role)
		val board = Board.empty.place(piece, init).get
		val set = new Moves(board, White).legalMoves.map(_.dest).toSet

		assert(set === refs)
	}

	test("check knight move in empty board") {
		checkMove(Knight, C3, Set(D1, E2, E4, D5, B5, A4, A2, B1))
		checkMove(Knight, A1, Set(B3, C2))
	}

	test("check king move in empty board") {
		checkMove(King, C3, Set(C2, D2, D3, D4, C4, B4, B3, B2))
		checkMove(King, H1, Set(G1, G2, H2))
	}

	test("check bishop move in empty board") {
		checkMove(Bishop, D4, Set(C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))
		checkMove(Bishop, A8, Set(B7, C6, D5, E4, F3, G2, H1))
	}

	test("check rook move in empty board") {
		checkMove(Rook, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8))
		checkMove(Rook, H8, Set(H7, H6, H5, H4, H3, H2, H1, A8, B8, C8, D8, E8, F8, G8))
	}

	test("check queen move in empty board") {
		checkMove(Queen, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8,
								C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))
		checkMove(Queen, H8, Set(H7, H6, H5, H4, H3, H2, H1, A8, B8, C8, D8, E8, F8, G8,
								A1, B2, C3, D4, E5, F6, G7))
	}

	test("check pawn move in empty board") {
		checkMove(Pawn, D2, Set(D3, D4))
		checkMove(Pawn, D3, Set(D4))
	}

}
