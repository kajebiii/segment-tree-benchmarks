package ck.benchmarks

import java.util.concurrent.TimeUnit
import scala.language.{ higherKinds, postfixOps }
import cats.data.{Chain, NonEmptyChain}
import org.openjdk.jmh.annotations.{ State => S, _ }
import zio.internal.Platform
import zio.{ BootstrapRuntime, Ref, Runtime, ZEnv, ZLayer }

import ck.benchmarks.Test._
import ck.benchmarks.ZioInstances._
import ck.entities._

@S(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class Benchmarks {

  private val runtime: Runtime[ZEnv] = new BootstrapRuntime {
    override val platform: Platform = Platform.benchmark
  }

  private val maxX = 10
  private val maxY = 10
  private val chunkWidth = 16
  private val chunkHeight = 16
  private val structureWidth = 5
  private val structureHeight = 5

  private val townSegments: Chain[gamedata.TownSegment] =
    for {
      x <- Chain.fromSeq(0 until maxX)
      y <- Chain.fromSeq(0 until maxY)
      id = x * maxY + y
      chunk = Area(x * chunkWidth, y * chunkHeight, (x+1) * chunkWidth - 1, (y+1) * chunkHeight - 1)
    } yield gamedata.TownSegment(id, NonEmptyChain.one(chunk))

  private val unlockSegments: Set[Int] =
    Set((for {
      x <- Chain.fromSeq(0 until maxX)
      y <- Chain.fromSeq(0 until maxY)
      id = x * maxY + y
    } yield id).filter(_ % 2 == 0).toList: _*)

  private val structures: Chain[Structure] =
    for {
      x <- Chain.fromSeq(0 to (maxX + maxX / 10 * 2) * chunkWidth / structureWidth)
      y <- Chain.fromSeq(0 to (maxY + maxY / 10 * 2) * chunkHeight / structureHeight)
      coordinates = Coordinates(-maxX / 10 + x * structureWidth, -maxY / 10 + y * structureHeight)
      size = Size(structureWidth, structureHeight)
    } yield Structure(coordinates, size)

  private val sgTreeLayer =
    ZLayer.fromEffect(Ref.make(SgTreeState(unlockSegments))) ++
      ZLayer.fromEffect(Ref.make(Chain.empty[SgTreeEvent])) ++
      ZLayer.succeed(SgTreeEnv(gamedata.SgTreeTown.fromNonEmptyChain(NonEmptyChain.fromChain(townSegments).get)))

  private val baseLayer =
    ZLayer.fromEffect(Ref.make(BaseState(unlockSegments))) ++
      ZLayer.fromEffect(Ref.make(Chain.empty[BaseEvent])) ++
      ZLayer.succeed(BaseEnv(gamedata.BaseTown(townSegments)))

  @Benchmark
  def benchmarkSgTreeForall(): Unit =
    runtime.unsafeRun(testSgTreeForall[SgTreeP](structures).provideLayer(sgTreeLayer))

  @Benchmark
  def benchmarkSgTreeGetAll(): Unit =
    runtime.unsafeRun(testSgTreeGetAll[SgTreeP](structures).provideLayer(sgTreeLayer))

  @Benchmark
  def benchmarkSgTreeFindAndContains(): Unit =
    runtime.unsafeRun(testSgTreeFindAndContains[SgTreeP](structures).provideLayer(sgTreeLayer))

  @Benchmark
  def benchmarkBaseFind(): Unit =
    runtime.unsafeRun(testBaseFind[BaseP](structures).provideLayer(baseLayer))

  @Benchmark
  def benchmarkBaseFilter(): Unit =
    runtime.unsafeRun(testBaseFilter[BaseP](structures).provideLayer(baseLayer))

//    def test(): Unit = {
//      val a = runtime.unsafeRun(testSgTreeForall[SgTreeP](structures).provideLayer(sgTreeLayer))
//      val b = runtime.unsafeRun(testSgTreeGetAll[SgTreeP](structures).provideLayer(sgTreeLayer))
//      val c = runtime.unsafeRun(testSgTreeFindAndContains[SgTreeP](structures).provideLayer(sgTreeLayer))
//      val d = runtime.unsafeRun(testBaseFind[BaseP](structures).provideLayer(baseLayer))
//      val e = runtime.unsafeRun(testBaseFilter[BaseP](structures).provideLayer(baseLayer))
//
//      val res = structures.toList.zipWithIndex.forall { case (_, index) =>
//        if (a.get(index) != b.get(index)) false
//        else if (c.get(index) != b.get(index)) false
//        else if (c.get(index) != d.get(index)) false
//        else if (e.get(index) != d.get(index)) false
//        else true
//      }
//      println("finish", res)
//
//      ()
//    }
}
