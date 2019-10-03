package algo

import org.scalatest.FunSuite
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.TicTacToeBrain
import checkers._


class TournamentTest extends FunSuite {
	
	test("basic tournament with Tic Tac Toe") {
		val players = List(
						TicTacToeAutoPlayer.alphaBetaPlayer,
						TicTacToeAutoPlayer.bestMovePlayer,
						TicTacToeAutoPlayer.minMaxPlayer,
						TicTacToeAutoPlayer.randomPlayer
					)

		val results = Tournament.tournament(initialGrid, Cross, Circle, players, TicTacToe.win, TicTacToe.draw)

		val rankedPlayers = results._1
		val gamesResults = results._2

		assert(rankedPlayers.size === players.size)

		// Check the number of games
		assert(gamesResults.size === players.size * (players.size - 1))

		// Check that random player is the last one
		assert(rankedPlayers.indexOf(TicTacToeAutoPlayer.randomPlayer) === rankedPlayers.size - 1)

		// Check that random player lost all of its games
		val winnersAgainstRandom = gamesResults.filter(res => res.winner == TicTacToeAutoPlayer.randomPlayer)
		assert(winnersAgainstRandom.isEmpty)
	}

}
