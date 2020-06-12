package ck
package entities

case class Area(x1: Int, y1: Int, x2: Int, y2: Int) { lhs =>
  import Area._

  require(x1 <= x2 && y1 <= y2, s"$x1, $x2, $y1, $y2")

  def intersects(rhs: Area): Boolean =
    segmentIntersects(lhs.x1, lhs.x2, rhs.x1, rhs.x2) &&
      segmentIntersects(lhs.y1, lhs.y2, rhs.y1, rhs.y2)

  def contains(coordinates: Coordinates): Boolean =
    x1 <= coordinates.x && coordinates.x <= x2 &&
      y1 <= coordinates.y && coordinates.y <= y2

  def contains(area: Area): Boolean =
    x1 <= area.x1 && area.x2 <= x2 &&
      y1 <= area.y1 && area.y2 <= y2

  def corners: (Coordinates, Coordinates, Coordinates, Coordinates) =
    (
      Coordinates(x1, y1),
      Coordinates(x1, y2),
      Coordinates(x2, y1),
      Coordinates(x2, y2)
    )

  def cornersSeq: Seq[Coordinates] =
    Seq(
      Coordinates(x1, y1),
      Coordinates(x1, y2),
      Coordinates(x2, y1),
      Coordinates(x2, y2)
    )

  def divide: (Area, Area, Area, Area) = {
    val xm = (x1 + x2) / 2
    val ym = (y1 + y2) / 2

    (
      Area(x1, y1, xm, ym),
      Area(x1, ym + 1, xm, y2),
      Area(xm + 1, y1, x2, ym),
      Area(xm + 1, ym + 1, x2, y2)
    )
  }
}

object Area {
  private[Area] def segmentIntersects(a1: Int, a2: Int, b1: Int, b2: Int): Boolean =
    a1 <= b2 && b1 <= a2

  @throws[java.lang.IllegalArgumentException]("if width and height are not non-negative integers.")
  def from(x: Int, y: Int, width: Int, height: Int): Area = {
    require(width >= 1 && height >= 1)
    Area(x, y, x + width - 1, y + height - 1)
  }

  @throws[java.lang.IllegalArgumentException]("if width and height are not non-negative integers.")
  def from(x: Int, y: Int, size: Size): Area =
    from(x, y, size.width, size.height)

  @throws[java.lang.IllegalArgumentException]("if width and height are not non-negative integers.")
  def from(coordinates: Coordinates, width: Int, height: Int): Area =
    from(coordinates.x, coordinates.y, width, height)

  @throws[java.lang.IllegalArgumentException]("if width and height are not non-negative integers.")
  def from(coordinates: Coordinates, size: Size): Area =
    from(coordinates, size.width, size.height)
}
