package cats.derived

import cats.kernel.CommutativeSemigroup
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive CommutativeSemigroup for ${A}.
Make sure it is a case class where all fields form CommutativeSemigroup.""")
type DerivedCommutativeSemigroup[A] = Derived[CommutativeSemigroup[A]]
object DerivedCommutativeSemigroup:
  type Or[A] = Derived.Or[CommutativeSemigroup[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: CommutativeSemigroup[A] =
    import DerivedCommutativeSemigroup.given
    summonInline[DerivedCommutativeSemigroup[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: CommutativeSemigroup[A] =
    import Strict.given
    summonInline[DerivedCommutativeSemigroup[A]].instance

  given [A](using inst: => ProductInstances[Or, A]): DerivedCommutativeSemigroup[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: CommutativeSemigroup[x], A](using @unused inst: ProductInstances[F, A])
      extends DerivedSemigroup.Product[F, A],
        CommutativeSemigroup[A]

  object Strict:
    given product[A: ProductInstancesOf[CommutativeSemigroup]]: DerivedCommutativeSemigroup[A] =
      new Product[CommutativeSemigroup, A] {}
