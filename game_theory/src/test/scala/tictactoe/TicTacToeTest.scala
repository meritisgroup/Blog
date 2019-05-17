package tictactoe

import org.scalatest.FunSuite
import TicTacToe._

class TicTacToeTest extends FunSuite {

	trait Level1 {
		val grid = List(Cross, Circle, Empty,
						Empty, Circle, Cross,
						Cross, Empty, Empty).toVector
	}

	trait Level2 {
		val grid = List(Empty, Empty, Empty,
						Empty, Cross, Empty,
						Empty, Empty, Empty).toVector
	}

	test("legal moves from initial") {
		assert(legalMoves(initialGrid, Cross).size === 9)
		assert(legalMoves(initialGrid, Circle).size === 9)
	}

	test("legal moves from level 1") {
		new Level1 {
			val emptyMarks = grid.filter(p => p == Empty).size

			assert(legalMoves(grid, Cross).size === emptyMarks)
			assert(legalMoves(grid, Circle).size === emptyMarks)

			assert(legalMoves(grid, Cross).map(_._2).sorted === List(2, 3, 7, 8))
			assert(legalMoves(grid, Circle).map(_._2).sorted === List(2, 3, 7, 8))
		}
	}

	test("win initial") {
		assert(!win(initialGrid, Cross))
		assert(!win(initialGrid, Circle))
	}

	test("win level 1") {
		new Level1 {
			assert(legalMoves(grid, Circle) exists (g => win(g._1, Circle)))
			assert(!win(grid, Cross))
		}
	}

	test("win level2") {
		new Level2 {
			assert(!win(grid, Cross))
		}
	}

}
