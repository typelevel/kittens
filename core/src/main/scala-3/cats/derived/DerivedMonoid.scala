package cats.derived

import cats.Monoid
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Monoid for ${A}.
Make sure it is a case class where all fields form Monoid.""")
type DerivedMonoid[A] = Derived[Monoid[A]]
object DerivedMonoid:
  @nowarn("msg=unused import")
  inline def apply[A]: Monoid[A] =
    import DerivedMonoid.given
    summonInline[DerivedMonoid[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Monoid[A] =
    import Strict.given
    summonInline[DerivedMonoid[A]].instance

  given [A](using inst: => ProductInstances[Derived.Or0[Monoid], A]): DerivedMonoid[A] =
    Strict.product

  trait Product[F[x] <: Monoid[x], A](using inst: ProductInstances[F, A])
      extends DerivedSemigroup.Product[F, A],
        Monoid[A]:
    final override lazy val empty: A =
      inst.construct([a] => (F: F[a]) => F.empty)

  object Strict:
    given product[A: ProductInstancesOf[Monoid]]: DerivedMonoid[A] =
      new Product[Monoid, A] {}
