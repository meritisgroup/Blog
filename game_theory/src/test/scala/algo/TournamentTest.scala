package algo

import org.scalatest.FunSuite
import tictactoe.TicTacToe
import tictactoe.TicTacToe._
import tictactoe.TicTacToeBrain


class TournamentTest extends FunSuite {
	
	test("basic tournament with Tic Tac Toe") {
		val players = List(
						TicTacToeAutoPlayer.alphaBetaPlayer _,
						TicTacToeBrain.bestMove _,
						TicTacToeAutoPlayer.minMaxPlayer _,
						TicTacToeAutoPlayer.randomPlayer _
					)

		val results = Tournament.tournament(initialGrid, Cross, Circle, players, TicTacToe.win, TicTacToe.draw)

		val rankedPlayers = results._1
		val gamesResults = results._2

		assert(rankedPlayers.size === players.size)

		// Check the number of games
		assert(gamesResults.size === players.size * (players.size - 1))

		// Check that random player is the last one
		assert(rankedPlayers.indexOf(players(3)) === rankedPlayers.size - 1)

		// Check that random player lost all of its games
		val winner1 = gamesResults.filter(res => res.player1 == players(3)).map(res => res.winner)
		assert(winner1.toSet == Set(Some(Circle)) || winner1.toSet == Set(Some(Circle), None))
		val winner2 = gamesResults.filter(res => res.player2 == players(3)).map(res => res.winner)
		assert(winner2.toSet == Set(Some(Cross)) || winner2.toSet == Set(Some(Cross), None))
	}

}
