package checkers

import org.scalatest.FunSuite


class BoardTest extends FunSuite {

	test("board evaluation") {
		assert(Board.init.evaluate(White) === 0)
		assert(Board.init.evaluate(Black) === 0)
	}

}
