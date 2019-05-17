package chess


sealed trait Color {

	val white = this == Color.White
	val black = this == Color.Black

	val unary_! : Color

}

object Color {

	case object White extends Color {
		val unary_! = Black

		override val hashCode = 1

	}

	case object Black extends Color {
		val unary_! = White

		override val hashCode = 2

	}

	def apply(b: Boolean): Color = if (b) White else Black

}
