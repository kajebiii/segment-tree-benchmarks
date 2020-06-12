package ck
package entities
package gamedata

import cats.data.{Chain, NonEmptyChain}

case class TownSegment(
  id: Int,
  chunks: NonEmptyChain[Area]
)
