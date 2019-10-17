package chess


case class Piece(color: Color, role: Role) {

	def is(c: Color) = c == color
	def is(r: Role) = r == role
	def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

}

object Piece {
	val all: List[Piece] = Role.all.flatMap(role => List(Piece(White, role), Piece(Black, role)))

}
