package cats.derived

import cats.Semigroup
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Semigroup for ${A}.
Make sure it is a case class where all fields form Semigroup.""")
type DerivedSemigroup[A] = Derived[Semigroup[A]]
object DerivedSemigroup:
  @nowarn("msg=unused import")
  inline def apply[A]: Semigroup[A] =
    import DerivedSemigroup.given
    summonInline[DerivedSemigroup[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Semigroup[A] =
    import Strict.given
    summonInline[DerivedSemigroup[A]].instance

  given [A](using inst: => ProductInstances[Derived.Or0[Semigroup], A]): DerivedSemigroup[A] =
    Strict.product

  trait Product[F[x] <: Semigroup[x], A](using inst: ProductInstances[F, A]) extends Semigroup[A]:
    final override def combine(x: A, y: A): A =
      inst.map2(x, y)([a] => (F: F[a], x: a, y: a) => F.combine(x, y))

  object Strict:
    given product[A: ProductInstancesOf[Semigroup]]: DerivedSemigroup[A] =
      new Product[Semigroup, A] {}
