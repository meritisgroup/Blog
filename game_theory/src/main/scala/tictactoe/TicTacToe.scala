package tictactoe


object TicTacToe {

	abstract class Mark
	case object Cross extends Mark
	case object Circle extends Mark
	case object Empty extends Mark

	def opponent(s: Mark): Mark = if (s == Cross) Circle else Cross

	// 0-1-2
	// 3-4-5
	// 6-7-8
	type Location = Int

	type Grid = Vector[Mark]

	val winningCombinations = List(
		Set(0, 1, 2), Set(3, 4, 5), Set(6, 7, 8),
		Set(0, 3, 6), Set(1, 4, 7), Set(2, 5, 8),
		Set(0, 4, 8), Set(2, 4, 6)
	)

	val initialGrid: Grid = List.fill(9)(Empty).toVector

	val allMarks = (0 until 9).toList

	def legalMoves(grid: Grid, side: Mark): List[(Grid, Location)] = {
		for (n <- allMarks if grid(n) == Empty) yield (grid.updated(n, side), n)
	}

	def win(grid: Grid, side: Mark): Boolean = {
		val set = allMarks.filter(n => grid(n) == side).toSet
		if (set.isEmpty) false
		else winningCombinations.exists(l => l.subsetOf(set))
	}
	
}
