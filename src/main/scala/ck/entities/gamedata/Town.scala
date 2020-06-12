package ck
package entities
package gamedata

import cats.data._
import cats.implicits._

case class Town(segments: NonEmptyMap[Int, TownSegment]) {
  val segmentTree: SegmentTree = {
    val chunksWithSegmentId: NonEmptyList[(Area, Int)] =
      segments.toNel.flatMap {
        case (segmentId, townSegment) =>
          townSegment.chunks.toNonEmptyList.map(_ -> segmentId)
      }
    val boundary: Area                                    =
      chunksWithSegmentId.map(_._1).reduceLeft[Area] {
        case (lhs, rhs) =>
          Area(lhs.x1 min rhs.x1, lhs.y1 min rhs.y1, lhs.x2 max rhs.x2, lhs.y2 max rhs.y2)
      }

    chunksWithSegmentId.foldLeft[SegmentTree](SegmentTree.Single(boundary, None)) {
      case (acc, (chunk, townSegmentId)) =>
        acc.addChunk(chunk, townSegmentId)
    }
  }

  def segment(dataId: Int): Option[TownSegment] =
    segments.lookup(dataId)
}

object Town {
  def fromNonEmptyChain(segments: NonEmptyChain[TownSegment]): Town =
    apply(segments.toNonEmptyMapBy(_.id))
}

case class TownSegment(
  id: Int,
  isUnlocked: Boolean,
  prerequisiteSegments: Chain[Int],
  chunks: NonEmptyChain[Area]
)

/**
  * y +
  *   |
  * 8 |
  * 7 | +--++--+
  * 6 | |lu||ru|
  * 5 | +--++--+
  * 4 | +--++--+
  * 3 | |ld||rd|
  * 2 | +--++--+
  * 1 |
  * 0 +----------+
  *   0123456789 x
  *
  * @author kajebiii
  */

sealed trait SegmentTree {
  def area: Area
  def addChunk(chunk: Area, townSegmentId: Int): SegmentTree
  def findIntersectSegmentId(area: Area, targetSegmentIds: Set[Int]): Option[Int]
}

object SegmentTree {
  case class Single(area: Area, townSegmentId: Option[Int]) extends SegmentTree {
    def addChunk(chunk: Area, townSegmentId: Int): SegmentTree =
      if (chunk contains area) copy(townSegmentId = Some(townSegmentId))
      else divide.addChunk(chunk, townSegmentId)

    def divide: Divided = {
      val (ld, lu, rd, ru) = area.divide

      Divided(
        area,
        Single(ld, townSegmentId),
        Single(lu, townSegmentId),
        Single(rd, townSegmentId),
        Single(ru, townSegmentId)
      )
    }

    def findIntersectSegmentId(area: Area, targetSegmentIds: Set[Int]): Option[Int] =
      if (this.area.intersects(area) && townSegmentId.exists(targetSegmentIds contains)) townSegmentId
      else None
  }

  case class Divided(
    area: Area,
    ld: SegmentTree,
    lu: SegmentTree,
    rd: SegmentTree,
    ru: SegmentTree
  ) extends SegmentTree {
    def addChunk(chunk: Area, townSegmentId: Int): SegmentTree =
      if (chunk contains area) Single(area, townSegmentId = Some(townSegmentId))
      else
        copy(
          ld = ld.addChunk(chunk, townSegmentId),
          lu = lu.addChunk(chunk, townSegmentId),
          rd = rd.addChunk(chunk, townSegmentId),
          ru = ru.addChunk(chunk, townSegmentId)
        )

    def findIntersectSegmentId(area: Area, targetSegmentIds: Set[Int]): Option[Int] =
      if (this.area.intersects(area))
        ld.findIntersectSegmentId(area, targetSegmentIds) orElse
          lu.findIntersectSegmentId(area, targetSegmentIds) orElse
          rd.findIntersectSegmentId(area, targetSegmentIds) orElse
          ru.findIntersectSegmentId(area, targetSegmentIds)
      else
        None
  }
}
