package chess


case class History(whiteShortCastlingAllowed: Boolean = true,
					whiteLongCastlingAllowed: Boolean = true,
					blackShortCastlingAllowed: Boolean = true,
					blackLongCastlingAllowed: Boolean = true) {
	
	def allowed(color: Color, longSide: Boolean): Boolean = {
		if (color == White) {
			if (longSide) whiteLongCastlingAllowed else whiteShortCastlingAllowed
		} else {
			if (longSide) blackLongCastlingAllowed else blackShortCastlingAllowed
		}
	}

	def kingMoved(color: Color): History = {
		if (color == White) {
			copy(whiteShortCastlingAllowed = false, whiteLongCastlingAllowed = false)
		} else {
			copy(blackShortCastlingAllowed = false, blackLongCastlingAllowed = false)
		}
	}

	def rookMoved(color: Color, origin: Pos): History = {
		if (color == White) {
			if (origin.x == 1) copy(whiteLongCastlingAllowed = false)
			else if (origin.x == 8) copy(whiteShortCastlingAllowed = false)
			else this
		} else {
			if (origin.x == 1) copy(blackLongCastlingAllowed = false)
			else if (origin.x == 8) copy(blackShortCastlingAllowed = false)
			else this
		}
	}

}
