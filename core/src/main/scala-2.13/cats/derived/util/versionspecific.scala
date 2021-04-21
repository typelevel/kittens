package cats.derived.util

import scala.annotation.implicitNotFound
import scala.util.hashing.MurmurHash3

object VersionSpecific {

  private[derived] def productSeed(x: Product): Int =
    MurmurHash3.mix(MurmurHash3.productSeed, x.productPrefix.hashCode)

  @implicitNotFound("could not find Lazy implicit value of type ${A}")
  abstract class Lazy[+A] extends Serializable {
    def value: A
  }

  object Lazy {
    implicit def instance[A](implicit ev: => A): Lazy[A] = new Lazy[A] {
      def value: A = ev
    }
  }

  sealed trait OrElse[+A, +B] extends Serializable {
    def fold[C](prim: A => C, sec: B => C): C
    def unify[C >: A](implicit ev: B <:< C): C = fold(identity, ev)
  }

  final class Primary[+A](value: A) extends OrElse[A, Nothing] {
    def fold[C](prim: A => C, sec: Nothing => C): C = prim(value)
  }

  final class Secondary[+B](value: => B) extends OrElse[Nothing, B] {
    def fold[C](prim: Nothing => C, sec: B => C): C = sec(value)
  }

  object OrElse extends OrElse0 {
    implicit def primary[A, B](implicit a: A): A OrElse B = new Primary(a)
  }

  abstract private[util] class OrElse0 {
    implicit def secondary[A, B](implicit b: => B): A OrElse B = new Secondary(b)
  }
}
