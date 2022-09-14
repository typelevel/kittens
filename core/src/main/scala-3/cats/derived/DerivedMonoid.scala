package cats.derived

import cats.Monoid
import shapeless3.deriving.K0

import scala.annotation.implicitNotFound
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Monoid[A] where A = ${A}.
Make sure that A is a case class where all fields have a Monoid instance.""")
type DerivedMonoid[A] = Derived[Monoid[A]]
object DerivedMonoid:
  type Or[A] = Derived.Or[Monoid[A]]
  inline def apply[A]: Monoid[A] =
    import DerivedMonoid.given
    summonInline[DerivedMonoid[A]].instance

  given [A](using inst: => K0.ProductInstances[Or, A]): DerivedMonoid[A] =
    given K0.ProductInstances[Monoid, A] = inst.unify
    new Product[Monoid, A] {}

  trait Product[F[x] <: Monoid[x], A](using inst: K0.ProductInstances[F, A])
      extends DerivedSemigroup.Product[F, A],
        Monoid[A]:
    final override lazy val empty: A =
      inst.construct([A] => (F: F[A]) => F.empty)
