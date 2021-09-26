package cats.derived

import cats.kernel.CommutativeMonoid
import shapeless3.deriving.K0

import scala.compiletime.*

type DerivedCommutativeMonoid[A] = Derived[CommutativeMonoid[A]]
object DerivedCommutativeMonoid:
  type Or[A] = Derived.Or[CommutativeMonoid[A]]
  inline def apply[A]: CommutativeMonoid[A] =
    import DerivedCommutativeMonoid.given
    summonInline[DerivedCommutativeMonoid[A]].instance

  given [A](using inst: K0.ProductInstances[Or, A]): DerivedCommutativeMonoid[A] =
    given K0.ProductInstances[CommutativeMonoid, A] = inst.unify
    new Product[CommutativeMonoid, A] {}

  trait Product[F[x] <: CommutativeMonoid[x], A](using inst: K0.ProductInstances[F, A])
      extends DerivedMonoid.Product[F, A],
        CommutativeMonoid[A]
