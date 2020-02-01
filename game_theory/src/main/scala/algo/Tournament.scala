package algo

import scala.annotation.tailrec


object Tournament {
	
	type Player[State, Side] = (State, Side) => State

	type EndFct[State, Side] = (State, Side) => Boolean

	case class GameResult[State, Side](player1: Player[State, Side],
										player2: Player[State, Side],
										winner: Option[Player[State, Side]])

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

}
