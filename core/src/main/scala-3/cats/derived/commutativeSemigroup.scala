package cats.derived

import cats.kernel.CommutativeSemigroup
import shapeless3.deriving.{K0, Continue}

trait ProductCommutativeSemigroup[T[x] <: CommutativeSemigroup[x], A](using inst: K0.ProductInstances[T, A])
    extends ProductSemigroup[T, A], CommutativeSemigroup[A] {}

trait CommutativeSemigroupDerivation:
  extension (F: CommutativeSemigroup.type)
    inline def derived[A](using gen: K0.ProductGeneric[A]): CommutativeSemigroup[A] =
      new ProductCommutativeSemigroup[CommutativeSemigroup, A]{}
