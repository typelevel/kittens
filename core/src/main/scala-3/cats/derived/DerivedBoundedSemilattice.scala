package cats.derived

import cats.kernel.BoundedSemilattice
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive BoundedSemilattice for ${A}.
Make sure it is a case class where all fields form BoundedSemilattice.""")
type DerivedBoundedSemilattice[A] = Derived[BoundedSemilattice[A]]
object DerivedBoundedSemilattice:
  @nowarn("msg=unused import")
  inline def apply[A]: BoundedSemilattice[A] =
    import DerivedBoundedSemilattice.given
    summonInline[DerivedBoundedSemilattice[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: BoundedSemilattice[A] =
    import Strict.given
    summonInline[DerivedBoundedSemilattice[A]].instance

  given product[A](using inst: => ProductInstances[BoundedSemilattice |: Derived, A]): DerivedBoundedSemilattice[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: BoundedSemilattice[x], A: ProductInstancesOf[F]]
      extends DerivedCommutativeMonoid.Product[F, A],
        BoundedSemilattice[A]

  object Strict:
    given product[A: ProductInstancesOf[BoundedSemilattice]]: DerivedBoundedSemilattice[A] =
      new Product[BoundedSemilattice, A] {}
