package algo

import org.scalatest.FunSuite
import tictactoe.TicTacToe._
import tictactoe.PlayTicTacToe._
import tictactoe._
import algo.TreeSearchAlgo._


class MinMaxTest extends FunSuite {


//
// Test on fake trees
//

	trait Level1 {
		val initial = Node(0, List(Node(1), Node(2), Node(3)))
	}

	trait Level2 {
		val child1 = Node(0.5, List(Node(1), Node(2), Node(3)))
		val child2 = Node(-0.5, List(Node(1.1), Node(2.6)))
		val initial = Node(0, List(child1, child2))
	}

	trait Level3 {
		val child1 = Node(0, List(Node(4.1), Node(5)))
		val father1 = Node(1, List(child1))
		val child2 = Node(2, List(Node(4), Node(6)))
		val father2 = Node(3, List(child2))
		val initial = Node(4, List(father1, father2))
	}

	trait Level4 {
		val child1 = Node(0, List(Node(1), Node(2), Node(3)))
		val child2 = Node(0, List(Node(1), Node(4)))
		val child3 = Node(0, List(Node(0.5), Node(0.6)))
		val initial = Node(0, List(child1, child2, child3))
	}

	test("1 level of children") {
		// Algo should pick up the highest one
		new Level1 {
			val best = MinMax.findBestNode[Node](initial, new NodeRules, HyperParameters(1))
			assert(best === SearchResult[Node](3, List(Node(3))))
		}
	}

	test("2 levels of children") {
		new Level2 {
			// Algo should pick up the node at depth 1 with the child assigned to the lowest value (ie 1.1 vs 1 => child2)
			val best = MinMax.findBestNode[Node](initial, new NodeRules, HyperParameters(2))
			assert(best === SearchResult[Node](1.1, List(child2, (Node(1.1)))))

			// Try with only 1 level of depth to check if fathers are correctly evaluated
			val bestWith1Depth = MinMax.findBestNode[Node](initial, new NodeRules, HyperParameters(1))
			assert(bestWith1Depth === SearchResult[Node](0.5, List(child1)))
		}
	}

	test("3 levels") {
		new Level3 {
			// Make sure that on the 3rd level, max is applied (and not min)
			val best = MinMax.findBestNode[Node](initial, new NodeRules, HyperParameters(3))
			assert(best === SearchResult[Node](6, List(father2, child2, Node(6))))
		}
	}

	test("pick up random node") {
		new Level4 {
			// As child1 and child2 have the same evaluation, they should be returned by the algo randomly
			val result = for (n <- 0 until 1000) yield MinMax.findBestNode[Node](initial, new NodeRules, HyperParameters(5))
			val set = result.map(r => r.bestChilds.head).toSet
			assert(set contains child1)
			assert(set contains child2)
		}
	}


//
// Test based on Tic Tac Toe game
//

	test("test specific grid") {
		// X - X
		// - O -
		// - - O
		val side = Circle
		val rules = new TicTacToeRules(side)
		val gridToTest = initialGrid.updated(0, Cross).updated(2, Cross).updated(4, Circle).updated(8, Circle)

		// Do it 10 times to make sure there is no random result
		for (i <- 0 to 10) {
			val result = MinMax.findBestNode[Grid](gridToTest, rules, HyperParameters(20))

			assert(!result.bestChilds.isEmpty)
			assert(result.bestChilds.head === gridToTest.updated(1, Circle))
		}
	}

	test("test specific grid (bis)") {
		// X - O
		// O O -
		// X - -
		val side = Cross
		val rules = new TicTacToeRules(side)
		val gridToTest = initialGrid.updated(0, Cross).updated(2, Circle).updated(3, Circle).updated(4, Circle).updated(6, Cross)

		// Do it 10 times to make sure there is no random result
		for (i <- 0 to 10) {
			val result = MinMax.findBestNode[Grid](gridToTest, rules, HyperParameters(20))

			assert(!result.bestChilds.isEmpty)
			assert(result.bestChilds.head === gridToTest.updated(5, Cross))
		}
	}

	test("test with tic tac toe : brain vs min max") {
		// Do it 10 times to make sure there is no random result
		for (i <- 0 to 10) {
			val result = TicTacToeAutoPlayer.play(Circle, initialGrid, TicTacToeAutoPlayer.minMaxPlayer, TicTacToeAutoPlayer.bestMovePlayer)
			if (result._1 == Cross) {
				result._2.reverse.foreach { grid => println(gridToString(grid)) }
				fail("MinMax was defeated by the brain when playing 1st")
			}

			val result2 = TicTacToeAutoPlayer.play(Circle, initialGrid, TicTacToeAutoPlayer.bestMovePlayer, TicTacToeAutoPlayer.minMaxPlayer)
			if (result2._1 == Circle) {
				result2._2.reverse.foreach { grid => println(gridToString(grid)) }
				fail("MinMax was defeated by the brain when playing 2nd")
			}
		}
	}

}
