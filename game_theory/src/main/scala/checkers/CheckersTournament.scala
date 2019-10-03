package checkers

import algo.Tournament
import algo.Tournament._
import algo.Player
import algo.AlphaBeta
import algo.HyperParameters


object CheckersTournament extends App {
	
	def buildPlayer(depth: Int, pawnSideWeight: Double, queenValue: Double): Player[Board, Color] = {
		val brain = new CheckersBrain(AlphaBeta.findBestNode, HyperParameters(depth), EvalParameters(pawnSideWeight, queenValue))
		val desc = "{depth " + depth + " pawn weight " + pawnSideWeight + " queen " + queenValue + "}"
		CheckersAutoPlayer.brainToPlayer(brain, desc)
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

	def tournament(players: List[Player[Board, Color]], movesLimit: Int): (List[Player[Board, Color]], List[GameResult[Board, Color]]) =
		Tournament.tournament(Board.init, White, Black, players, CheckersAutoPlayer.win, CheckersAutoPlayer.draw, movesLimit)

	val weakest = buildPlayer(2, 1.0, 2.0)

	val players = List(
			buildPlayer(6, 1.00, 10.0),
			//buildPlayer(6, 1.02, 10.0),
			//buildPlayer(6, 1.04, 10.0),
			//buildPlayer(6, 1.06, 10.0),
			//buildPlayer(6, 1.08, 10.0),
			buildPlayer(6, 1.10, 10.0),
			//buildPlayer(6, 1.12, 10.0),
			//buildPlayer(6, 1.14, 10.0),
			//buildPlayer(6, 1.16, 10.0),
			//buildPlayer(6, 1.18, 10.0),
			buildPlayer(6, 1.20, 10.0)
		)

	val round1 = players.filter { player => play(weakest, player, 200)._1 == Some(Black) }

	println(round1.size + " players passed 1 round")
	round1.foreach(player => println(player.desc))

	println("tournament started")
	val results = tournament(round1, 500)

	println("ranking")
	for (i <- 0 until results._1.size) {
		println("rank " + (i+1) + " : " + results._1(i).desc)
	}
	
}
