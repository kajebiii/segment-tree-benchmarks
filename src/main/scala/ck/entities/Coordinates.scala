package ck
package entities

case class Coordinates(x: Int, y: Int) {
  def adjacentTo(other: Coordinates): Boolean =
    Math.abs(x - other.x) + Math.abs(y - other.y) <= 1
}

object Coordinates {
  val origin: Coordinates =
    Coordinates(0, 0)
}
