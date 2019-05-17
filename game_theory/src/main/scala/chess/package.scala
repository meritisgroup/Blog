package chess

package object chess {
	type Direction = Pos => Option[Pos]
	type Directions = List[Direction]
}
