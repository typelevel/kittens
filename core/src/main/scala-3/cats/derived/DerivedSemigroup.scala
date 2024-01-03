package cats.derived

import cats.Semigroup
import shapeless3.deriving.K0

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Semigroup[A] where A = ${A}.
Make sure that A is a case class where all fields have a Semigroup instance.""")
type DerivedSemigroup[A] = Derived[Semigroup[A]]
object DerivedSemigroup:
  type Or[A] = Derived.Or[Semigroup[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Semigroup[A] =
    import DerivedSemigroup.given
    summonInline[DerivedSemigroup[A]].instance

  given [A](using inst: => K0.ProductInstances[Or, A]): DerivedSemigroup[A] =
    given K0.ProductInstances[Semigroup, A] = inst.unify
    new Product[Semigroup, A] {}

  trait Product[F[x] <: Semigroup[x], A](using inst: K0.ProductInstances[F, A]) extends Semigroup[A]:
    final override def combine(x: A, y: A): A =
      inst.map2(x, y)([A] => (F: F[A], x: A, y: A) => F.combine(x, y))
