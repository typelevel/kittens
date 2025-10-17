package cats.derived

import cats.kernel.Semilattice
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Semilattice for ${A}.
Make sure it is a case class where all fields form Semilattice.""")
type DerivedSemilattice[A] = Derived[Semilattice[A]]
object DerivedSemilattice:
  inline def apply[A]: Semilattice[A] =
    import DerivedSemilattice.given
    summonInline[DerivedSemilattice[A]].instance

  inline def strict[A]: Semilattice[A] =
    import Strict.given
    summonInline[DerivedSemilattice[A]].instance

  given product[A](using inst: => ProductInstances[Semilattice |: Derived, A]): DerivedSemilattice[A] =
    Strict.product(using inst.unify)

  @nowarn("msg=unused implicit parameter")
  trait Product[F[x] <: Semilattice[x], A: ProductInstancesOf[F]]
      extends DerivedCommutativeSemigroup.Product[F, A],
        Semilattice[A]

  object Strict:
    given product[A: ProductInstancesOf[Semilattice]]: DerivedSemilattice[A] =
      new Product[Semilattice, A] {}
