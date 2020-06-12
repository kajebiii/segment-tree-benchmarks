import cats.Order
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.implicits._

package object ck {
  implicit class NecOps[A](val nec: NonEmptyChain[A]) extends AnyVal {
    def max(implicit cmp: Ordering[A]): A =
      nec.toList.max

    def min(implicit cmp: Ordering[A]): A =
      nec.toList.min

    def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A =
      nec.toList.maxBy(f)

    def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A =
      nec.toList.minBy(f)

    def toNonEmptyMapBy[K](f: A => K)(implicit K: Order[K]): NonEmptyMap[K, A] = {
      val (head, tail) = nec.map(a => f(a) -> a).uncons

      NonEmptyMap.of(head, tail.toList: _*)
    }
  }

  implicit class COps[A](val chain: Chain[A]) extends AnyVal {
    def toSeq: Seq[A] =
      chain.iterator.toSeq

    def toSet[B >: A]: Set[B] =
      chain.iterator.toSet

    def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] =
      chain.iterator.toMap

    def toMapBy[K](f: A => K): Map[K, A] =
      chain.map(a => f(a) -> a).iterator.toMap
  }
}
