package algo

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.PlayTicTacToe._


class AlphaBetaTest extends AlgoTest {

	case class Node(eval: Double, children: List[Node] = Nil)

	def eval(node: Node, maximize: Boolean): Double = node.eval
	def children(node: Node, maximize: Boolean): List[Node] = node.children

	def evalAndCount(counter: AtomicInteger): (Node, Boolean) => Double = {
		def tmp(node: Node, maximize: Boolean): Double = {
			counter.incrementAndGet
			node.eval
		}

		tmp
	}

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

	trait LevelAlphaCut {
		val child1 = Node(5)
		val child2 = Node(0, List(Node(4), Node(5), Node(6), Node(7), Node(8)))
		val initial = Node(0, List(child1, child2))
	}

	trait LevelBetaCut {
		val child1 = Node(3)
		val child2 = Node(0, List(Node(4), Node(5), Node(6), Node(7), Node(8)))
		val initial1 = Node(0, List(child1, child2))
		val initial = Node(0, List(initial1))
	}

	test("same results as min max") {
		new Level1 {
			val bestMinMax = MinMax.findBestNode[Node](initial, 10, eval, children)
			val bestAlphaBeta = AlphaBeta.findBestNode[Node](initial, 10, eval, children)
			assert(bestMinMax.value === bestAlphaBeta.value)
			assert(bestMinMax.bestChilds === bestAlphaBeta.bestChilds)
		}
		new Level2 {
			val bestMinMax = MinMax.findBestNode[Node](initial, 10, eval, children)
			val bestAlphaBeta = AlphaBeta.findBestNode[Node](initial, 10, eval, children)
			assert(bestMinMax.value === bestAlphaBeta.value)
			assert(bestMinMax.bestChilds === bestAlphaBeta.bestChilds)
		}
		new Level3 {
			val bestMinMax = MinMax.findBestNode[Node](initial, 10, eval, children)
			val bestAlphaBeta = AlphaBeta.findBestNode[Node](initial, 10, eval, children)
			assert(bestMinMax.value === bestAlphaBeta.value)
			assert(bestMinMax.bestChilds === bestAlphaBeta.bestChilds)
		}
		new LevelAlphaCut {
			val bestMinMax = MinMax.findBestNode[Node](initial, 10, eval, children)
			val bestAlphaBeta = AlphaBeta.findBestNode[Node](initial, 10, eval, children)
			assert(bestMinMax.value === bestAlphaBeta.value)
			assert(bestMinMax.bestChilds === bestAlphaBeta.bestChilds)
		}
		new LevelBetaCut {
			val bestMinMax = MinMax.findBestNode[Node](initial, 10, eval, children)
			val bestAlphaBeta = AlphaBeta.findBestNode[Node](initial, 10, eval, children)
			assert(bestMinMax.value === bestAlphaBeta.value)
			assert(bestMinMax.bestChilds === bestAlphaBeta.bestChilds)
		}
	}

	test("test with tic tac toe : min max vs alpha beta") {
		// Do it 10 times to make sure there is no random result
		for (i <- 0 to 10) {
			val result = play(Circle, initialGrid, playerMinMax, playerAlphaBeta)
			if (result._1 == Circle) {
				result._2.reverse.foreach { grid => println(gridToString(grid)) }
				fail("AlphaBeta was defeated by MinMax when playing 1st")
			}

			val result2 = play(Circle, initialGrid, playerAlphaBeta, playerMinMax)
			if (result2._1 == Cross) {
				result2._2.reverse.foreach { grid => println(gridToString(grid)) }
				fail("AlphaBeta was defeated by MinMax when playing 2nd")
			}
		}
	}

	test("alpha cut") {
		// See following page to check example tree
		// https://fr.wikipedia.org/wiki/%C3%89lagage_alpha-b%C3%AAta#Principe for
		new LevelAlphaCut {
			val evalCount = new AtomicInteger(0)
			AlphaBeta.findBestNode[Node](initial, 10, evalAndCount(evalCount), children)
			assert(evalCount.get === 2)
		}
	}

	test("beta cut") {
		// See following page to check example tree
		// https://fr.wikipedia.org/wiki/%C3%89lagage_alpha-b%C3%AAta#Principe for 
		// Added a root node compared to Wikipedia to simulate the context with maximize=false
		new LevelBetaCut {
			val evalCount = new AtomicInteger(0)
			AlphaBeta.findBestNode[Node](initial, 10, evalAndCount(evalCount), children)
			assert(evalCount.get === 2)
		}
	}

}
