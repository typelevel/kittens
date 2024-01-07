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

  @nowarn("msg=unused import")
  inline def strict[A]: Semigroup[A] =
    import Strict.given
    summonInline[DerivedSemigroup[A]].instance

  given product[A](using inst: => K0.ProductInstances[Or, A]): DerivedSemigroup[A] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedSemigroup_A[A](using => K0.ProductInstances[Or, A]): DerivedSemigroup[A] = product

  trait Product[F[x] <: Semigroup[x], A](using inst: K0.ProductInstances[F, A]) extends Semigroup[A]:
    final override def combine(x: A, y: A): A =
      inst.map2(x, y)([a] => (F: F[a], x: a, y: a) => F.combine(x, y))

  object Strict:
    given product[A](using K0.ProductInstances[Semigroup, A]): DerivedSemigroup[A] =
      new Product[Semigroup, A] {}
