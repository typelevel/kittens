package cats.derived

import cats.Monoid
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Monoid[A] where A = ${A}.
Make sure that A is a case class where all fields have a Monoid instance.""")
type DerivedMonoid[A] = Derived[Monoid[A]]
object DerivedMonoid:
  type Or[A] = Derived.Or[Monoid[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Monoid[A] =
    import DerivedMonoid.given
    summonInline[DerivedMonoid[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Monoid[A] =
    import Strict.given
    summonInline[DerivedMonoid[A]].instance

  given [A](using inst: => ProductInstances[Or, A]): DerivedMonoid[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: Monoid[x], A](using inst: ProductInstances[F, A])
      extends DerivedSemigroup.Product[F, A],
        Monoid[A]:
    final override lazy val empty: A =
      inst.construct([A] => (F: F[A]) => F.empty)

  object Strict:
    given product[A: ProductInstancesOf[Monoid]]: DerivedMonoid[A] =
      new Product[Monoid, A] {}
