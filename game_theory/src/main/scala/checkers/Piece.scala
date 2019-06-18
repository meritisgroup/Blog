package checkers


sealed trait Color {

	val white = this == White
	val black = this == Black

	val unary_! : Color

}

case object White extends Color {

	val unary_! = Black
	override val hashCode = 1

}

case object Black extends Color {

	val unary_! = White
	override val hashCode = 2

}


sealed trait Role

case object Pawn extends Role {
	override val hashCode = 1
}

case object Queen extends Role {
	override val hashCode = 2
}


sealed case class Pos private (m: Int) {

	lazy val downLeft: Option[Pos] = Pos.manoury(Pos.row(m) + 1, Pos.col(m) - 1).flatMap(Pos.posAt(_))
	lazy val downRight: Option[Pos] = Pos.manoury(Pos.row(m) + 1, Pos.col(m) + 1).flatMap(Pos.posAt(_))
	lazy val upLeft: Option[Pos] = Pos.manoury(Pos.row(m) - 1, Pos.col(m) - 1).flatMap(Pos.posAt(_))
	lazy val upRight: Option[Pos] = Pos.manoury(Pos.row(m) - 1, Pos.col(m) + 1).flatMap(Pos.posAt(_))

	override val hashCode = m

}


object Pos {

	val posCache = new Array[Some[Pos]](50)

	def posAt(m: Int): Option[Pos] = {
		if (m < 1 || m > 50) None
		else posCache(m - 1)
	}

	def row(m: Int): Int = (m - 1) / 5
	def col(m: Int): Int = ((m - 1) % 5) * 2 + ((row(m) + 1) % 2)
	def manoury(row: Int, col: Int): Option[Int] = {
		if (row < 0 || row > 9 || col < 0 || col > 9) None
		else if ((row % 2 == 0 && col % 2 == 0) || (row % 2 == 1 && col % 2 == 1)) None
		else Some(row * 5 + col / 2 + 1)
	}

	private[this] def createPos(m: Int): Pos = {
		val pos = new Pos(m)
		posCache(m - 1) = Some(pos)
		pos
	}

	val all: List[Pos] = (1 to 50).map(createPos(_)).toList

}


case class Piece(color: Color, role: Role) {

	def is(c: Color) = c == color
	def is(r: Role) = r == role

	override lazy val hashCode = role.hashCode * 2 + color.hashCode

}


sealed trait Direction {

	def apply(at: Pos): Option[Pos]
	def perpendiculars: List[Direction]

}

case object UpAndLeft extends Direction {

	def apply(at: Pos) = at.upLeft
	lazy val perpendiculars = List(UpAndLeft, UpAndRight, DownAndLeft)
	override val hashCode = 1

}

case object UpAndRight extends Direction {

	def apply(at: Pos) = at.upRight
	lazy val perpendiculars = List(UpAndRight, UpAndLeft, DownAndRight)
	override val hashCode = 2

}

case object DownAndLeft extends Direction {

	def apply(at: Pos) = at.downLeft
	lazy val perpendiculars = List(DownAndLeft, UpAndLeft, DownAndRight)
	override val hashCode = 3

}

case object DownAndRight extends Direction {

	def apply(at: Pos) = at.downRight
	lazy val perpendiculars = List(DownAndRight, UpAndLeft, DownAndRight)
	override val hashCode = 4

}

object Direction {

	val whitePawnMoves: List[Direction] = List(UpAndLeft, UpAndRight)
	val blackPawnMoves: List[Direction] = List(DownAndLeft, DownAndRight)

	val all: List[Direction] = List(UpAndLeft, UpAndRight, DownAndLeft, DownAndRight)

}
