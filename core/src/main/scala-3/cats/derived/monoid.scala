package cats.derived

import cats.Monoid
import shapeless3.deriving.K0

trait ProductMonoid[F[x] <: Monoid[x], A](using
    inst: K0.ProductInstances[F, A]
) extends ProductSemigroup[F, A],
      Monoid[A]:
  val empty: A = inst.construct([A] => (F: F[A]) => F.empty)
