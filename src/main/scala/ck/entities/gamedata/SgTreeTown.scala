package ck
package entities
package gamedata

import cats.data._
import cats.implicits._

case class SgTreeTown(segments: NonEmptyMap[Int, TownSegment]) {
  val segmentTree: AreaTree[Int] =
    AreaTree.apply(
      segments.toNel.flatMap {
        case (segmentId, townSegment) =>
          townSegment.chunks.toNonEmptyList.map(_ -> segmentId)
      }
    )

  def segment(dataId: Int): Option[TownSegment] =
    segments.lookup(dataId)
}

object SgTreeTown {
  def fromNonEmptyChain(segments: NonEmptyChain[TownSegment]): SgTreeTown =
    apply(segments.toNonEmptyMapBy(_.id))
}

