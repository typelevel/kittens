package cats.derived

import cats.kernel.CommutativeMonoid
import shapeless3.deriving.K0

object commutativeMonoid extends CommutativeMonoidDerivation

trait ProductCommutativeMonoid[T[x] <: CommutativeMonoid[x], A](using inst: K0.ProductInstances[T, A])
    extends ProductCommutativeSemigroup[T, A], ProductMonoid[T, A], CommutativeMonoid[A] {}

trait CommutativeMonoidDerivation:
  extension (F: CommutativeMonoid.type)
    inline def derived[A](using gen: K0.ProductGeneric[A]): CommutativeMonoid[A] =
      new ProductCommutativeMonoid[CommutativeMonoid, A]{}
