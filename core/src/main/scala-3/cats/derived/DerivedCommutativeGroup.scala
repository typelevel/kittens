package cats.derived

import cats.kernel.CommutativeGroup
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive CommutativeGroup for ${A}.
Make sure it is a case class where all fields form CommutativeGroup.""")
type DerivedCommutativeGroup[A] = Derived[CommutativeGroup[A]]
object DerivedCommutativeGroup:
  inline def apply[A]: CommutativeGroup[A] =
    import DerivedCommutativeGroup.given
    summonInline[DerivedCommutativeGroup[A]].instance

  inline def strict[A]: CommutativeGroup[A] =
    import Strict.given
    summonInline[DerivedCommutativeGroup[A]].instance

  given product[A](using inst: => ProductInstances[CommutativeGroup |: Derived, A]): DerivedCommutativeGroup[A] =
    Strict.product(using inst.unify)

  @nowarn("msg=unused implicit parameter")
  trait Product[F[x] <: CommutativeGroup[x], A: ProductInstancesOf[F]]
      extends DerivedGroup.Product[F, A],
        CommutativeGroup[A]

  object Strict:
    given product[A: ProductInstancesOf[CommutativeGroup]]: DerivedCommutativeGroup[A] =
      new Product[CommutativeGroup, A] {}
