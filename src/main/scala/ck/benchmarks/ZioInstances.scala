package ck.benchmarks

import cats.data.Chain
import cats.kernel.Monoid
import cats.mtl._
import cats.{ Applicative, Functor, Monad }
import ck.benchmarks.Test._
import zio.{ Has, Ref, Tag, ZIO }

object ZioInstances {

  type ZIOReaderWriterState[E, L, S, +A] = ZIO[Has[E] with Has[Ref[S]] with Has[Ref[L]], Nothing, A]

  implicit def zioApplicativeAsk[E: Tag, L, S](
    implicit ev: Applicative[ZIOReaderWriterState[E, L, S, *]],
    monoid: Monoid[L]
  ): ApplicativeAsk[ZIOReaderWriterState[E, L, S, *], E] =
    new DefaultApplicativeAsk[ZIOReaderWriterState[E, L, S, *], E] {
      override val applicative: Applicative[ZIOReaderWriterState[E, L, S, *]] = ev
      override def ask: ZIOReaderWriterState[E, L, S, E]                      = ZIO.service[E]
    }

  implicit def zioFunctorTell[E, L, S](
    implicit ev: Functor[ZIOReaderWriterState[E, L, S, *]],
    monoid: Monoid[L],
    tag: Tag[Ref[L]]
  ): FunctorTell[ZIOReaderWriterState[E, L, S, *], L] =
    new DefaultFunctorTell[ZIOReaderWriterState[E, L, S, *], L] {
      override val functor: Functor[ZIOReaderWriterState[E, L, S, *]] = ev
      override def tell(l: L): ZIOReaderWriterState[E, L, S, Unit] =
        ZIO.accessM[Has[Ref[L]]](_.get.update(log => monoid.combine(log, l)))
    }

  implicit def zioMonadState[E, L, S](
    implicit ev: Monad[ZIOReaderWriterState[E, L, S, *]],
    tag: Tag[Ref[S]]
  ): MonadState[ZIOReaderWriterState[E, L, S, *], S] =
    new DefaultMonadState[ZIOReaderWriterState[E, L, S, *], S] {
      override val monad: Monad[ZIOReaderWriterState[E, L, S, *]] = ev
      override def get: ZIOReaderWriterState[E, L, S, S]          = ZIO.accessM[Has[Ref[S]]](_.get.get)
      override def set(s: S): ZIOReaderWriterState[E, L, S, Unit] = ZIO.accessM[Has[Ref[S]]](_.get.set(s))
    }

  implicit def zioMonad[E, L, S](implicit monoid: Monoid[L]): Monad[ZIOReaderWriterState[E, L, S, *]] =
    new Monad[ZIOReaderWriterState[E, L, S, *]] {
      override def pure[A](x: A): ZIOReaderWriterState[E, L, S, A] = ZIO.succeed(x)
      override def flatMap[A, B](fa: ZIOReaderWriterState[E, L, S, A])(
        f: A => ZIOReaderWriterState[E, L, S, B]
      ): ZIOReaderWriterState[E, L, S, B] = fa.flatMap(f)
      override def tailRecM[A, B](
        a: A
      )(f: A => ZIOReaderWriterState[E, L, S, Either[A, B]]): ZIOReaderWriterState[E, L, S, B] = f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => ZIO.succeed(b)
      }
    }

  implicit val m1: Monad[SgTreeP]                     = zioMonad[SgTreeEnv, Chain[SgTreeEvent], SgTreeState]
  implicit val m2: ApplicativeAsk[SgTreeP, SgTreeEnv]       = zioApplicativeAsk[SgTreeEnv, Chain[SgTreeEvent], SgTreeState]
  implicit val m3: FunctorTell[SgTreeP, Chain[SgTreeEvent]] = zioFunctorTell[SgTreeEnv, Chain[SgTreeEvent], SgTreeState]
  implicit val m4: MonadState[SgTreeP, SgTreeState]         = zioMonadState[SgTreeEnv, Chain[SgTreeEvent], SgTreeState]

  implicit val m5: Monad[BaseP]                     = zioMonad[BaseEnv, Chain[BaseEvent], BaseState]
  implicit val m6: ApplicativeAsk[BaseP, BaseEnv]       = zioApplicativeAsk[BaseEnv, Chain[BaseEvent], BaseState]
  implicit val m7: FunctorTell[BaseP, Chain[BaseEvent]] = zioFunctorTell[BaseEnv, Chain[BaseEvent], BaseState]
  implicit val m8: MonadState[BaseP, BaseState]         = zioMonadState[BaseEnv, Chain[BaseEvent], BaseState]
}
