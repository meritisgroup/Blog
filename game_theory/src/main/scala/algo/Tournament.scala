package algo

import scala.annotation.tailrec


case class Player[State, Side](play: (State, Side) => State, desc: String)


object Tournament {
	
	type EndFct[State, Side] = (State, Side) => Boolean

	case class GameResult[State, Side](player1: Player[State, Side], player2: Player[State, Side], winner: Option[Player[State, Side]])

	@tailrec
	def play[State, Side](currentState: State, side1: Side, side2: Side,
							player1: Player[State, Side], player2: Player[State, Side],
							win: EndFct[State, Side], draw: EndFct[State, Side],
							movesLimit: Int = 15,
							list: List[State] = Nil): (Option[Side], List[State]) = {

		if (list.size >= movesLimit) {
			(None, list.reverse)

		} else {
			val nextState = player1.play(currentState, side1)

			if (win(nextState, side1)) {
				(Some(side1), (nextState :: list).reverse)

			} else if (draw(nextState, side1)) {
				(None, (nextState :: list).reverse)

			} else {
				play(nextState, side2, side1, player2, player1, win, draw, movesLimit, nextState :: list)
			}
		}
	}

	def tournament[State, Side](initialState: State, side1: Side, side2: Side,
								players: List[Player[State, Side]],
								win: EndFct[State, Side], draw: EndFct[State, Side],
								movesLimit: Int = 100): (List[Player[State, Side]], List[GameResult[State, Side]]) = {

		def game(player1: Player[State, Side], player2: Player[State, Side]): GameResult[State, Side] = {
			val result = play[State, Side](initialState, side1, side2, player1, player2, win, draw, movesLimit)
			val winner = result._1 map { side => if (side == side1) player1 else player2 }
			GameResult(player1, player2, winner)
		}

		def loggedGame(player1: Player[State, Side], player2: Player[State, Side]): GameResult[State, Side] = {
			print(player1.desc + " vs " + player2.desc + " ... ")
			val result = game(player1, player2)
			println("winner is " + (result.winner.map(_.desc)))
			result
		}

		def buildTasks: List[() => GameResult[State, Side]] = {
			for (player1 <- players; player2 <- players if player1 != player2) yield (() => loggedGame(player1, player2))
		}

		def score(player: Player[State, Side], results: List[GameResult[State, Side]]): Int = {
			results.filter(res => res.winner == player).size
		}

		val tasks = buildTasks

		// Execute games in parallel
		//val results = tasks.par.map(_()).toList
		// Execute in a single thread
		val results = tasks.map(_()).toList

		val list = players.sortBy { player => - score(player, results) }

		(list, results)
	}

}
