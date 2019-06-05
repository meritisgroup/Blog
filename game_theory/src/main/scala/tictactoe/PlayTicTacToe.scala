package tictactoe

import scala.util.Random
import TicTacToe._


object PlayTicTacToe extends App {

	val allowedInputs = allMarks.map(_.toString).toSet

	val explanations = List("Please input a number between 0 and 8 to place your mark as follow :", "0-1-2", "3-4-5", "6-7-8")

	val playerSide = Cross
	val computerSide = Circle

	def markToChar(mark: Mark): Char = mark match {
		case Cross  => 'X'
		case Circle => 'O'
		case _      => '-'
	}

	def gridToString(grid: Grid): String = {
		List(grid.slice(0, 3), grid.slice(3, 6), grid.slice(6, 9))
			.map(row => row.map(markToChar).mkString(" "))
			.mkString("\n")
	}

	def letUserPlay(grid: Grid): Grid = {
		def play(n: Int): Grid = {
			val allowed = legalMoves(grid, playerSide)
			val move = allowed.filter { case (_, loc) => loc == n }
			if (!move.isEmpty) move.head._1
			else {
				println("Your move is not allowed.")
				println("Allowed moves are : " + legalMoves(grid, playerSide).map(_._2).mkString(", "))
				letUserPlay(grid)
			}
		}

		explanations foreach println

		println("Current grid (your mark is " + markToChar(playerSide) + ") :")
		println(gridToString(grid))

		println("Input your move :")
		val line = scala.io.StdIn.readLine()

		if (allowedInputs.contains(line)) play(line.toInt)
		else {
			println("The value " + line + " you input is not correct.")
			letUserPlay(grid)
		}
	}

	def letComputerPlay(grid: Grid): Grid = {
		TicTacToeBrain.bestMove(computerSide, grid)
	}

	def playRound(grid: Grid): Mark = {
		val gridAfterUser = letUserPlay(grid)
		println("Grid after player's move :\n" + gridToString(gridAfterUser))

		if (win(gridAfterUser, playerSide)) {
			playerSide
		} else {
			if (legalMoves(gridAfterUser, computerSide).isEmpty) {
				Empty
			} else {
				println("The computer plays")
				val gridAfterComputer = letComputerPlay(gridAfterUser)
				if (win(gridAfterComputer, computerSide)) {
					println("Grid after computer's move :\n" + gridToString(gridAfterComputer))
					computerSide
				} else playRound(gridAfterComputer)
			}
		}
	}

	// Start by player
	val winner = playRound(initialGrid)
	// Start by computer
	//val winner = playRound(letComputerPlay(initialGrid))

	if (winner == playerSide) println("The player won !")
	else if (winner == computerSide) println("The computer won !")
	else println("No move allowed anymore : draw !")

}
