package chess


case class Piece(color: Color, role: Role) {

	def is(c: Color) = c == color
	def is(r: Role) = r == role
	def forsyth: Char = if (color.white) role.forsythUpper else role.forsyth

}
