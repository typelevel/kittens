package cats.derived.util

import shapeless.HList
import shapeless.ops.function.{FnFromProduct, FnToProduct}

trait FnGeneric[F[_]] {
  type A <: HList
  def to[B](f: F[B]): A => B
  def from[B](f: A => B): F[B]
}

object FnGeneric {
  type x
  type Aux[F[_], L <: HList] = FnGeneric[F] { type A = L }

  implicit def instance[F[_], L <: HList](
    implicit toP: FnToProduct.Aux[F[x], L => x], fromP: FnFromProduct.Aux[L => x, F[x]]
  ): Aux[F, L] = new FnGeneric[F] {
    type A = L
    def to[B](f: F[B]): A => B = toP(f.asInstanceOf[F[x]]).asInstanceOf[A => B]
    def from[B](f: A => B): F[B] = fromP(f.asInstanceOf[A => x]).asInstanceOf[F[B]]
  }
}
