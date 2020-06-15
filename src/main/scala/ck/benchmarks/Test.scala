package ck.benchmarks

import scala.language.higherKinds
import cats.Monad
import cats.data.Chain
import cats.implicits._
import cats.mtl.{ApplicativeAsk, FunctorTell, MonadState}
import ck.benchmarks.ZioInstances.ZIOReaderWriterState
import ck.entities._

import scala.collection.immutable.TreeSet

object Test {
  case class SgTreeEnv(town: gamedata.SgTreeTown)
  case class SgTreeEvent()
  case class SgTreeState(unlockedSegments: Set[Int])

  case class BaseEnv(town: gamedata.BaseTown)
  case class BaseEvent()
  case class BaseState(unlockedSegments: Set[Int])

  type SgTreeP[+A] = ZIOReaderWriterState[SgTreeEnv, Chain[SgTreeEvent], SgTreeState, A]
  type BaseP[+A] = ZIOReaderWriterState[BaseEnv, Chain[BaseEvent], BaseState, A]

  def testSgTree[F[_]: Monad](structures: Chain[Structure])(
    implicit reader: ApplicativeAsk[F, SgTreeEnv],
    writer: FunctorTell[F, Chain[SgTreeEvent]],
    state: MonadState[F, SgTreeState]
  ): F[Unit] =
    structures
      .map { structure =>
        for {
          town <- reader.ask.map(_.town)
          townSegments = town.segments.toSortedMap
          segmentTree = town.segmentTree
          unlockedSegments <- state.get.map(_.unlockedSegments)
          lockedSegmentIds = (townSegments -- unlockedSegments).keySet
          area = Area.from(structure.coordinates, structure.size)
          invalidSegmentId = segmentTree.findIntersectSegmentId(area, lockedSegmentIds)
          isInUnlocked = invalidSegmentId.isEmpty && segmentTree.findIntersectSegmentId(area, townSegments.keySet).nonEmpty
        } yield isInUnlocked
      }
      .sequence
      .void

  def testBase[F[_]: Monad](structures: Chain[Structure])(
    implicit reader: ApplicativeAsk[F, BaseEnv],
    writer: FunctorTell[F, Chain[BaseEvent]],
    state: MonadState[F, BaseState]
  ): F[Unit] =
    structures
      .map { structure =>
        def findIntersectSegmentId(area: Area, townSegmentsData: Chain[gamedata.TownSegment]): Option[Int] =
          townSegmentsData
            .find(_.chunks.exists(_ intersects area))
            .map(_.id)

        def isNotOutsideSegments(area: Area, townSegmentsData: Chain[gamedata.TownSegment]): Boolean = {
          val points: Chain[Area] =
            for {
              x <- Chain.fromSeq(Set(area.x1, area.x2).toSeq)
              y <- Chain.fromSeq(Set(area.y1, area.y2).toSeq)
            } yield Area(x, y, x, y)

          points
            .traverse(findIntersectSegmentId(_, townSegmentsData))
            .forall(_.nonEmpty)
        }

        for {
          town <- reader.ask.map(_.town)
          townSegments = town.segments
          unlockedSegments <- state.get.map(_.unlockedSegments)
          lockedSegmentIds = Chain.fromSeq((townSegments -- unlockedSegments).values.toSeq)
          area = Area.from(structure.coordinates, structure.size)
          invalidSegmentId = findIntersectSegmentId(area, lockedSegmentIds)
          isInUnlocked = invalidSegmentId.isEmpty && isNotOutsideSegments(area, Chain.fromSeq(townSegments.values.toSeq))
        } yield isInUnlocked
      }
      .sequence
      .void

  def testBaseFilter[F[_]: Monad](structures: Chain[Structure])(
    implicit reader: ApplicativeAsk[F, BaseEnv],
    writer: FunctorTell[F, Chain[BaseEvent]],
    state: MonadState[F, BaseState]
  ): F[Unit] =
    structures
      .map { structure =>
        def findIntersectSegmentId(area: Area, townSegmentsData: Chain[gamedata.TownSegment]): Option[Int] =
          townSegmentsData
            .filter(_.chunks.exists(_ intersects area))
            .map(_.id)
            .headOption

        def isNotOutsideSegments(area: Area, townSegmentsData: Chain[gamedata.TownSegment]): Boolean = {
          val points: Chain[Area] =
            for {
              x <- Chain.fromSeq(Set(area.x1, area.x2).toSeq)
              y <- Chain.fromSeq(Set(area.y1, area.y2).toSeq)
            } yield Area(x, y, x, y)

          points
            .traverse(findIntersectSegmentId(_, townSegmentsData))
            .forall(_.nonEmpty)
        }

        for {
          town <- reader.ask.map(_.town)
          townSegments = town.segments
          unlockedSegments <- state.get.map(_.unlockedSegments)
          lockedSegmentIds = Chain.fromSeq((townSegments -- unlockedSegments).values.toSeq)
          area = Area.from(structure.coordinates, structure.size)
          invalidSegmentId = findIntersectSegmentId(area, lockedSegmentIds)
          isInUnlocked = invalidSegmentId.isEmpty && isNotOutsideSegments(area, Chain.fromSeq(townSegments.values.toSeq))
        } yield isInUnlocked
      }
      .sequence
      .void
}
