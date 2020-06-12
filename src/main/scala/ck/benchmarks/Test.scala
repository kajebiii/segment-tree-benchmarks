package ck.benchmarks

import scala.language.higherKinds
import cats.Monad
import cats.data.Chain
import cats.implicits._
import cats.mtl.{ ApplicativeAsk, FunctorTell, MonadState }
import ck.benchmarks.ZioInstances.ZIOReaderWriterState

object Test {
  case class Env(config: String)
  case class Event(name: String)
  case class State(value: Int)

  type P[+A] = ZIOReaderWriterState[Env, Chain[Event], State, A]

  val loops = 1000

//  def isInUnlockedSegments(structureRef: StructureRef)(implicit F: KingdomProgrammatic[F]): F[Boolean] =
//    for {
//      structureData    <- inquireStructure(structureRef.structure.dataId)
//      townSegments     <- inquireTownSegments
//      segmentTree      <- inquireSegmentTree
//      unlockedSegments <- getUnlockedTownSegments.map(_.keys)
//      lockedSegmentIds  = (townSegments -- unlockedSegments).keySet
//      area              = Area.from(structureRef.placement.coordinates, structureData.size, structureRef.placement.rotation)
//      invalidSegmentId  = segmentTree.findIntersectSegmentId(area, lockedSegmentIds)
//      isInUnlocked      = structureRef.placement.targetTown.isMyKingdom &&
//        invalidSegmentId.isEmpty &&
//        segmentTree.findIntersectSegmentId(area, townSegments.keySet).nonEmpty
//    } yield isInUnlocked

  def testMTL[F[_]: Monad](
    implicit reader: ApplicativeAsk[F, Env],
    writer: FunctorTell[F, Chain[Event]],
    state: MonadState[F, State]
  ): F[Unit] =
    (1 to loops).toList
      .map(_ =>
        for {
          conf <- reader.ask.map(_.config)
          _    <- writer.tell(Chain(Event(s"Env = $conf")))
          _    <- state.modify(state => state.copy(value = state.value + 1))
        } yield ()
      )
      .sequence
      .void
}
