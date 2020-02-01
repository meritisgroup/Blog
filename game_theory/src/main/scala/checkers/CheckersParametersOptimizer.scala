package checkers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.TimeZone

import algo.Tournament
import algo.Tournament._
import algo.AlphaBeta
import algo.HyperParameters


object CheckersParametersOptimizer extends App {
	
	def buildPlayer(parameters: (Int, Double, Double)): Player[Board, Color] = {
		val depth = parameters._1
		val pawnSideWeight = parameters._2
		val queenValue = parameters._3

		val brain = new CheckersBrain(AlphaBeta.findBestNode, HyperParameters(depth), EvalParameters(pawnSideWeight, queenValue))
		CheckersAutoPlayer.brainToPlayer(brain)
	}

	def toString(parameters: (Int, Double, Double)): String = {
		val depth = parameters._1
		val pawnSideWeight = parameters._2
		val queenValue = parameters._3

		f"{ depth=$depth pawn side=$pawnSideWeight%1.2f queen=$queenValue%2.0f }"
	}

	def printBoard(board: Board): Unit = {
		println("\n----- BOARD -----")

		val orderedPieces = board.pieces.values.groupBy(identity).mapValues(_.size)
		val leakestPieces = orderedPieces.filter(piece => piece._1.is(White))
		val strongestPieces = orderedPieces.filter(piece => piece._1.is(Black))
		println("Leakest player has " + leakestPieces.values.sum + " pieces left / Strongest player has " + strongestPieces.values.sum + " pieces left")

		println(PlayCheckers.boardToString(board))
	}

	def play(player1: Player[Board, Color], player2: Player[Board, Color], movesLimit: Int): (Option[Color], List[Board]) =
		Tournament.play(Board.init, White, Black, player1, player2, CheckersAutoPlayer.win, CheckersAutoPlayer.draw, movesLimit)


	// pawn side value between 1.1 and 1.3
	// queen value between 6 and 14
	// for depth 6, winning couples are (1.1, 1.18, 1.26) x (6, 8, 10, 12, 14)

	val weakest = (2, 1.0, 2.0)

	val depth = 6
	val pawnSideAverage = 1.1
	val queenAverage = 10

	val players = (for (ps <- 1.1 to 1.3 by 0.02; qa <- 6.0 to 14.0 by 2.0)
					yield ((depth, ps, qa))).toList

	TimeZone.setDefault(TimeZone.getTimeZone("Europe/Paris"))
	println("*** optimizer started at " + LocalDateTime.now.format(DateTimeFormatter.ofPattern("HH:mm")))

	println("*** round 1 starts")
	val checkPoint1 = System.currentTimeMillis()
	//val round1 = players.par.filter { player => play(buildPlayer(weakest), buildPlayer(player), 200)._1 == Some(Black) }
	val round1 = players.filter { player => play(buildPlayer(weakest), buildPlayer(player), 200)._1 == Some(Black) }
	val elapsed1 = (System.currentTimeMillis() - checkPoint1) / 1000

	round1.foreach(player => println(toString(player)))
	println("*** " + round1.size + " players passed 1 round in " + elapsed1 + " sec")

	println("*** round 2 starts")
	val checkPoint2 = System.currentTimeMillis()
	val tasks = for (p1 <- round1; p2 <- round1 if p1 != p2) yield {
		() => {
			println(toString(p1) + " vs " + toString(p2))
			val result = play(buildPlayer(p1), buildPlayer(p2), 200)
			if (result._1 == Some(White)) println("winner is " + toString(p1))
			else if (result._2 == Some(Black)) println("winner is " + toString(p2))
			else println("no winner at all")
		}
	}
	//tasks.par.map(_())
	tasks.map(_())

	val elapsed2 = (System.currentTimeMillis() - checkPoint2) / 1000
	println("*** 2nd round in " + elapsed2 + " sec")

}
