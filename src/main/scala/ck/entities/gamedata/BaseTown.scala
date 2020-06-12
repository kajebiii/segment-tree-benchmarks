package ck
package entities
package gamedata

import cats.data._
import cats.implicits._

case class BaseTown(segments: Map[Int, TownSegment]) {
  def segment(dataId: Int): Option[TownSegment] = segments.get(dataId)
}

object BaseTown {
  def apply(segments: Chain[TownSegment]): BaseTown = {
    apply(segments.toMapBy(_.id))
  }
}


