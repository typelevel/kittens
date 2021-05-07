package cats.derived

import cats.Monoid
import shapeless3.deriving.K0

object monoid extends MonoidDerivation

class ProductMonoid[F[x] <: Monoid[x], A](
  using inst: K0.ProductInstances[F, A]
) extends ProductSemigroup[F, A], Monoid[A]:
  val empty: A = inst.construct([A] => (F: F[A]) => F.empty)

trait MonoidDerivation:
  extension (M: Monoid.type)
    inline def derived[A](using gen: K0.ProductGeneric[A]): Monoid[A] =
      ProductMonoid(using K0.mkProductInstances)
