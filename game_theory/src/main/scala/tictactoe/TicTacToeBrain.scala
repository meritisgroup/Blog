package tictactoe

import TicTacToe._

object TicTacToeBrain {

	val middle = 4

	val corners = List(0, 2, 8, 6)

	def aboutToWin(side: Mark, grid: Grid): Option[Location] = {
		def test(set: Set[Location]): Option[Location] = {
			val grouped = set.groupBy(i => grid(i)).withDefaultValue(List())
			if (grouped(side).size == 2 && grouped(Empty).size == 1) {
				Some(grouped(Empty).head)
			} else None
		}

		val result = winningCombinations.flatMap(set => test(set))

		if (result.isEmpty) None
		else Some(result.head)
	}

	def bestMove(side: Mark, grid: Grid): Grid = {
		val marksCount = grid.filter(m => m != Empty).size
		val opposite = if (side == Cross) Circle else Cross

		// For the 1st moves, try to put a mark in the middle or in a corner
		if (marksCount <= 1) {
			if (grid(middle) == Empty) grid.updated(middle, side)
			else grid.updated(corners.head, side)

		} else if (marksCount == 2) {

			// If the player put a mark in a corner, fill the opposite corner
			if (corners.exists(i => grid(i) != Empty)) {
				val corner = corners.find(i => grid(i) != Empty)
				val oppositeCorner = corners((corner.get + 2) % corners.size)
				grid.updated(oppositeCorner, side)

			// Otherwise, fill the closest corner
			} else {
				val present = grid.indexOf(opposite)
				val corner = if (present < middle) 0 else 8
				grid.updated(corner, side)
			}

		} else {
			// If computer is about to win, end up the game !
			val win = aboutToWin(side, grid)
			if (!win.isEmpty) {
				grid.updated(win.get, side)
			} else {

				// If the player is about to win, block him !
				val winOpposite = aboutToWin(opposite, grid)
				if (!winOpposite.isEmpty) {
					grid.updated(winOpposite.get, side)
				} else {

					// Otherwise pick up an empty location (this case should never happen)
					val emptyCorner = corners.find(i => grid(i) == Empty)
					if (!emptyCorner.isEmpty) grid.updated(emptyCorner.get, side)
					else {
						val index = allMarks.find(i => grid(i) == Empty)
						grid.updated(index.get, side)
					}
				}
			}
		}
	}

}
