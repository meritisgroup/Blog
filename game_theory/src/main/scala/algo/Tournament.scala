package algo

import scala.annotation.tailrec


object Tournament {
	
	type Player[State, Side] = (State, Side) => State

	type EndFct[State, Side] = (State, Side) => Boolean

	case class GameResult[State, Side](player1: Player[State, Side], player2: Player[State, Side], winner: Option[Side])

	@tailrec
	def play[State, Side](currentState: State, side1: Side, side2: Side,
							player1: Player[State, Side], player2: Player[State, Side],
							win: EndFct[State, Side], draw: EndFct[State, Side],
							movesLimit: Int = 15,
							list: List[State] = Nil): (Option[Side], List[State]) = {

		if (list.size >= movesLimit) {
			(None, list.reverse)

		} else {
			val nextState = player1(currentState, side1)

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
			GameResult(player1, player2, result._1)
		}

		def buildTasks: List[() => GameResult[State, Side]] = {
			for (player1 <- players; player2 <- players if player1 != player2) yield (() => game(player1, player2))
		}

		def score(player: Player[State, Side], results: List[GameResult[State, Side]]): Int = {
			val score1 = results.filter(res => res.player1 == player && res.winner == Some(side1)).size
			val score2 = results.filter(res => res.player2 == player && res.winner == Some(side2)).size

			score1 + score2
		}

		val tasks = buildTasks
		val results = tasks.par.map(_()).toList
		val list = players.sortBy { player => - score(player, results) }

		(list, results)
	}

}
