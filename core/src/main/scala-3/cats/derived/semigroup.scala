package cats.derived

import cats.Semigroup
import shapeless3.deriving.K0

trait ProductSemigroup[F[x] <: Semigroup[x], A](using
    inst: K0.ProductInstances[F, A]
) extends Semigroup[A]:
  def combine(x: A, y: A): A =
    inst.map2(x, y)([A] => (F: F[A], x: A, y: A) => F.combine(x, y))
