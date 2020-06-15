package ck.benchmarks

import java.util.concurrent.TimeUnit
import scala.language.{ higherKinds, postfixOps }
import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import org.openjdk.jmh.annotations.{ State => S, _ }
import zio.internal.Platform
import zio.{ BootstrapRuntime, Ref, Runtime, ZEnv, ZLayer }
import ck.benchmarks.Test._
import ck.benchmarks.ZioInstances._
import ck.entities._

@S(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class Benchmarks {

  private val runtime: Runtime[ZEnv] = new BootstrapRuntime {
    override val platform: Platform = Platform.benchmark
  }

  private val maxX = 10
  private val maxY = 10
  private val chunkWidth = 6
  private val chunkHeight = 6

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
      x <- Chain.fromSeq(-maxX/10 until maxX+maxX/10)
      y <- Chain.fromSeq(-maxY/10 until maxY+maxY/10)
      coordinates = Coordinates(x * chunkWidth, y * chunkHeight)
      size = Size(chunkWidth, chunkHeight)
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
  def benchmarkSgTree(): Unit =
    runtime.unsafeRun(testSgTree[SgTreeP](structures).provideLayer(sgTreeLayer))

  @Benchmark
  def benchmarkBase(): Unit =
    runtime.unsafeRun(testBase[BaseP](structures).provideLayer(baseLayer))

  @Benchmark
  def benchmarkBaseFilter(): Unit =
    runtime.unsafeRun(testBaseFilter[BaseP](structures).provideLayer(baseLayer))
}
