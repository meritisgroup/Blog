package chess

import org.scalatest.FunSuite
import Pos._


class PosTest extends FunSuite {

	def checkNeighbors(orig: Pos, up: Pos, down: Pos, left: Pos, right: Pos,
						dl: Pos, dr: Pos, ur: Pos, ul: Pos) = {

		assert(orig.up == Some(up))
		assert(orig.down == Some(down))
		assert(orig.left == Some(left))
		assert(orig.right == Some(right))
		assert(orig.downLeft === Some(dl))
		assert(orig.downRight === Some(dr))
		assert(orig.upRight === Some(ur))
		assert(orig.upLeft === Some(ul))
	}

	test("pos down/up left/right neighbors") {
		checkNeighbors(D4, D5, D3, C4, E4, C3, E3, E5, C5)
	}

}
