package chess

import chess._


sealed trait Role {

	val forsyth: Char
	val forsythUpper: Char = forsyth.toUpper

	val dirs: Directions

}

case object King extends Role {
	val forsyth = 'k'

	val dirs = Queen.dirs

}

case object Queen extends Role {
	val forsyth = 'q'

	val dirs = Rook.dirs ::: Bishop.dirs

}

// Rook = Tour
case object Rook extends Role {
	val forsyth = 'r'

	val dirs = List(_.up, _.down, _.left, _.right)

}

// Bishop = Fou
case object Bishop extends Role {
	val forsyth = 'b'

	val dirs = List(_.upLeft, _.upRight, _.downLeft, _.downRight)

}

// Knight = Cavalier
case object Knight extends Role {
	import Pos.posAt

	val forsyth = 'n'

	val dirs: Directions = List(
		p => posAt(p.x - 1, p.y + 2),
		p => posAt(p.x - 1, p.y - 2),
		p => posAt(p.x + 1, p.y + 2),
		p => posAt(p.x + 1, p.y - 2),
		p => posAt(p.x - 2, p.y + 1),
		p => posAt(p.x - 2, p.y - 1),
		p => posAt(p.x + 2, p.y + 1),
		p => posAt(p.x + 2, p.y - 1)
	)

}

// Pawn = Pion
case object Pawn extends Role {
	val forsyth = 'p'
	
	val dirs = Nil

}

object Role {
	val all: List[Role] = List(King, Queen, Rook, Bishop, Knight, Pawn)

}
