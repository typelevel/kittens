package cats.derived

import cats.kernel.BoundedSemilattice
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of BoundedSemilattice[A] where A = ${A}.
Make sure that A is a case class where all fields have a BoundedSemilattice instance.""")
type DerivedBoundedSemilattice[A] = Derived[BoundedSemilattice[A]]
object DerivedBoundedSemilattice:
  type Or[A] = Derived.Or[BoundedSemilattice[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: BoundedSemilattice[A] =
    import DerivedBoundedSemilattice.given
    summonInline[DerivedBoundedSemilattice[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: BoundedSemilattice[A] =
    import Strict.given
    summonInline[DerivedBoundedSemilattice[A]].instance

  given product[A](using inst: => ProductInstances[Or, A]): DerivedBoundedSemilattice[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: BoundedSemilattice[x], A: ProductInstancesOf[F]]
      extends DerivedCommutativeMonoid.Product[F, A],
        BoundedSemilattice[A]

  object Strict:
    given product[A: ProductInstancesOf[BoundedSemilattice]]: DerivedBoundedSemilattice[A] =
      new Product[BoundedSemilattice, A] {}
