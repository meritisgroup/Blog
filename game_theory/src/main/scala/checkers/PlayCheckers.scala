package checkers

import scala.util.Random
import algo.HyperParameters
import algo.AlphaBeta


object PlayCheckers extends App {

	val playerSide = White
	val computerSide = Black

	def pieceToChar(piece: Piece): Char = piece match {
		case Piece(White, Pawn)  => 'o'
		case Piece(Black, Pawn)  => 'x'
		case Piece(White, Queen)  => 'O'
		case Piece(Black, Queen)  => 'X'
	}

	def locToString(pos: Pos): String = {
		val row = 10 - Pos.row(pos.m)
		val col = ('a' + Pos.col(pos.m)).toChar
		col.toString + row.toString
	}

	def boardToString(board: Board): String = {
		def mToChar(loc: Option[Int]): Char = loc match {
			case Some(m) if board(Pos.posAt(m).get).isDefined => pieceToChar(board(Pos.posAt(m).get).get)
			case Some(m) => '.'
			case _ => ' '
		}

		val refLetters = "    a  b  c  d  e  f  g  h  i  j"

		val lines = for (row <- 0 to 9) yield {
			val rowText = 10 - row
			val chars = (0 to 9).map { col => mToChar(Pos.manoury(row, col)) }

			f"$rowText%2.0f" + "  " + chars.mkString("  ") + "  " + f"$rowText%2.0f"
		}

		val list = List(refLetters) ++ lines ++ List(refLetters)
		list.mkString("\n")
	}

	def moveToString(move: Move): String = {
		def paranthesis(text: String) = if (move.piece.color.black) "(" + text + ")" else text
		def cross = if (move.captureCount == 0) "-" else "x"

		paranthesis(locToString(move.from) + cross + locToString(move.to))
	}

	def parseInput(board: Board, input: String): Option[Move] = {
		new Moves(board, playerSide).legalMoves.find { move => moveToString(move) == input }
	}

	def letUserPlay(board: Board): Option[Move] = {
		println("Current board :")
		println
		println(boardToString(board))
		println

		println("Please input your move, your side is " + pieceToChar(Piece(playerSide, Pawn)))
		println("for instance, c3-d4 to move a pawn, g5xf6 to take a pawn :")

		val line = scala.io.StdIn.readLine()

		if (line == "exit") {
			None
		} else if (!parseInput(board, line).isEmpty) {
			parseInput(board, line)
		} else {
			println("Your move is not allowed.")
			println("Allowed moves are : " + new Moves(board, playerSide).legalMoves.map(moveToString(_)).mkString(", "))
			letUserPlay(board)
		}
	}

	def letComputerPlay(board: Board): Option[Move] = {
		val result = new CheckersBrain(AlphaBeta.findBestNode, HyperParameters(10)).bestMove(board, computerSide)
		result._1
	}

	def win(board: Board, sideToPlay: Color): Boolean = {
		new Moves(board, sideToPlay).win == Won
	}

	def playRound(board: Board): Option[Color] = {
		val boardAfterUser = letUserPlay(board)
		if (boardAfterUser.isDefined) println("Board after player's move :\n\n" + boardToString(boardAfterUser.get.after))

		if (boardAfterUser.isEmpty) {
			None
		} else if (new Moves(boardAfterUser.get.after, computerSide).win == Lost) {
			Some(playerSide)
		} else {
			print("\nThe computer plays")

			val startTime = System.currentTimeMillis()
			val boardAfterComputer = letComputerPlay(boardAfterUser.get.after)
			val elapsedTime = System.currentTimeMillis() - startTime

			if (boardAfterComputer.isDefined) {
				print(" " + moveToString(boardAfterComputer.get) + " in " + elapsedTime + " ms")
			}

			println

			if (boardAfterComputer.isEmpty) {
				None
			} else if (new Moves(boardAfterComputer.get.after, playerSide).win == Lost) {
				println("Board after computer's move :\n" + boardToString(boardAfterComputer.get.after))
				Some(computerSide)
			} else {
				playRound(boardAfterComputer.get.after)
			}
		}
	}

	// Start by player
	playRound(Board.init) match {
		case Some(side) if side == playerSide   => println("The player won !")
		case Some(side) if side == computerSide => println("The computer won !")
		case _ => println("No winner")
	}

}
