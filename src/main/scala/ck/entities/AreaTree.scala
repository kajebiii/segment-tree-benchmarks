package ck
package entities

import cats.data.NonEmptyList

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

sealed trait AreaTree[K] {
  def area: Area
  def addArea(area: Area, townSegmentId: K): AreaTree[K]
  // area 와 intersect 하는 keys 들 중, targetKeys 에 포함된 key 하나를 반환합니다.
  def findIntersectAreaKey(area: Area, targetKeys: Set[K]): Option[K]
  // key 가 등록된 area 영역들에 포함되어 있는지를 반환합니다.
  def contains(area: Area): Boolean
  // area 와 intersect 하는 영역의 모든 Option[K] 를 반환합니다.
  def getAllIntersectAreaKeys(area: Area): Set[Option[K]]
  // area 와 intersect 하는 모든 key 가 조건을 만족하는지 검사합니다.
  def forall(area: Area)(p: Option[K] => Boolean): Boolean
}

object AreaTree {
  def apply[K](areasWithKey: NonEmptyList[(Area, K)]): AreaTree[K] = {
    val boundaryFromChunks: Area =
      areasWithKey.map(_._1).reduceLeft[Area] {
        case (lhs, rhs) =>
          Area(lhs.x1 min rhs.x1, lhs.y1 min rhs.y1, lhs.x2 max rhs.x2, lhs.y2 max rhs.y2)
      }

    def getEqualOrGreaterPowerOfTwo(target: Int): Int = {
      @scala.annotation.tailrec
      def _getEqualOrGreaterPowerOfTwo(number: Int, result: Int): Int =
        if (number == 0) result
        else _getEqualOrGreaterPowerOfTwo(number / 2, result * 2)

      _getEqualOrGreaterPowerOfTwo(target - 1, 1)
    }

    val maxLength: Int =
      getEqualOrGreaterPowerOfTwo(
        ((boundaryFromChunks.x2 - boundaryFromChunks.x1 + 2) max (boundaryFromChunks.y2 - boundaryFromChunks.y1 + 2)) + 1
      )
    val boundary: Area =
      boundaryFromChunks.copy(
        boundaryFromChunks.x1 - 1,
        boundaryFromChunks.y1 - 1,
        boundaryFromChunks.x1 - 1 + maxLength - 1,
        boundaryFromChunks.y1 - 1 + maxLength - 1
      )

    areasWithKey.foldLeft[AreaTree[K]](AreaTree.Single[K](boundary, None)) {
      case (acc, (chunk, townSegmentId)) =>
        acc.addArea(chunk, townSegmentId)
    }
  }

  case class Single[K](area: Area, townSegmentId: Option[K]) extends AreaTree[K] {
    def addArea(area: Area, townSegmentId: K): AreaTree[K] =
      if (!this.area.intersects(area)) this
      else if (area contains this.area) copy(townSegmentId = Some(townSegmentId))
      else divide.addArea(area, townSegmentId)

    def divide: Divided[K] = {
      val (ld, lu, rd, ru) = area.divide

      Divided(
        area,
        Single(ld, townSegmentId),
        Single(lu, townSegmentId),
        Single(rd, townSegmentId),
        Single(ru, townSegmentId)
      )
    }

    def findIntersectAreaKey(area: Area, targetKeys: Set[K]): Option[K] =
      if (this.area.intersects(area)) {
        if (townSegmentId.exists(targetKeys contains)) townSegmentId
        else None
      } else None

    def contains(area: Area): Boolean =
      if (!this.area.intersects(area)) true
      else townSegmentId.isDefined

    def getAllIntersectAreaKeys(area: Area): Set[Option[K]] =
      if (!this.area.intersects(area)) Set.empty
      else Set(townSegmentId)

    def forall(area: Area)(p: Option[K] => Boolean): Boolean =
      if (!this.area.intersects(area)) true
      else p(townSegmentId)
  }

  case class Divided[K](
    area: Area,
    ld: AreaTree[K],
    lu: AreaTree[K],
    rd: AreaTree[K],
    ru: AreaTree[K]
  ) extends AreaTree[K] {
    def addArea(area: Area, townSegmentId: K): AreaTree[K] =
      if (!this.area.intersects(area)) this
      else if (area contains this.area) Single(area, townSegmentId = Some(townSegmentId))
      else
        copy(
          ld = ld.addArea(area, townSegmentId),
          lu = lu.addArea(area, townSegmentId),
          rd = rd.addArea(area, townSegmentId),
          ru = ru.addArea(area, townSegmentId)
        )

    def findIntersectAreaKey(area: Area, targetKeys: Set[K]): Option[K] =
      if (this.area.intersects(area))
        ld.findIntersectAreaKey(area, targetKeys) orElse
          lu.findIntersectAreaKey(area, targetKeys) orElse
          rd.findIntersectAreaKey(area, targetKeys) orElse
          ru.findIntersectAreaKey(area, targetKeys)
      else
        None

    def contains(area: Area): Boolean =
      if (!this.area.intersects(area)) true
      else ld.contains(area) && lu.contains(area) && rd.contains(area) && ru.contains(area)

    def getAllIntersectAreaKeys(area: Area): Set[Option[K]] =
      if (!this.area.intersects(area)) Set.empty
      else ld.getAllIntersectAreaKeys(area) ++ lu.getAllIntersectAreaKeys(area) ++ rd.getAllIntersectAreaKeys(area) ++ ru.getAllIntersectAreaKeys(area)

    def forall(area: Area)(p: Option[K] => Boolean): Boolean =
      if (!this.area.intersects(area)) true
      else ld.forall(area)(p) && lu.forall(area)(p) && rd.forall(area)(p) && ru.forall(area)(p)
  }
}
