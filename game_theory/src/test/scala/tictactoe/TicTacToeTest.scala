package tictactoe

import org.scalatest.FunSuite
import TicTacToe._


class TicTacToeTest extends FunSuite {

	trait Level1 {
		val grid = Vector(Cross, Circle, Empty,
						Empty, Circle, Cross,
						Cross, Empty, Empty)
	}

	trait Level2 {
		val grid = Vector(Empty, Empty, Empty,
						Empty, Cross, Empty,
						Empty, Empty, Empty)
	}

	trait Level3 {
		val grid = Vector(Cross, Circle, Empty,
						Circle, Circle, Empty,
						Cross, Empty, Empty)
	}

	test("legal moves from initial") {
		assert(legalMoves(initialGrid, Cross).size === 9)
		assert(legalMoves(initialGrid, Circle).size === 9)
	}

	test("legal moves from level 1") {
		new Level1 {
			val emptyMarks = grid.filter(_ == Empty).size

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

	test("brain 1st move") {
		val grid = TicTacToeBrain.bestMove(Circle, initialGrid)
		assert(grid(4) === Circle)
	}

	test("brain 2nd move") {
		new Level2 {
			val grid2 = TicTacToeBrain.bestMove(Circle, grid)
			assert(grid2(0) === Circle)
		}
	}

	test("brain : computer is about to win") {
		new Level1 {
			val grid2 = TicTacToeBrain.bestMove(Circle, grid)
			assert(grid2(7) === Circle)
			assert(win(grid2, Circle))
		}
	}

	test("brain : player is about to win") {
		new Level3 {
			val grid2 = TicTacToeBrain.bestMove(Cross, grid)
			assert(grid2(5) === Cross)
		}
	}

}
