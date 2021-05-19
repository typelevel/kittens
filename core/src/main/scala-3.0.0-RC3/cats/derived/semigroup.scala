package cats.derived

import cats.Semigroup
import shapeless3.deriving.K0

object semigroup extends SemigroupDerivation

trait ProductSemigroup[F[x] <: Semigroup[x], A](
  using inst: K0.ProductInstances[F, A]
) extends Semigroup[A]:
  def combine(x: A, y: A): A =
    inst.map2(x, y)([A] => (F: F[A], x: A, y: A) => F.combine(x, y))

trait SemigroupDerivation:
  extension (S: Semigroup.type)
    inline def derived[A](using gen: K0.ProductGeneric[A]): Semigroup[A] =
      new ProductSemigroup[Semigroup, A]{}
