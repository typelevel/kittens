package cats.derived

import cats.kernel.CommutativeMonoid
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive CommutativeMonoid for ${A}.
Make sure it is a case class where all fields form CommutativeMonoid.""")
type DerivedCommutativeMonoid[A] = Derived[CommutativeMonoid[A]]
object DerivedCommutativeMonoid:
  inline def apply[A]: CommutativeMonoid[A] =
    import DerivedCommutativeMonoid.given
    summonInline[DerivedCommutativeMonoid[A]].instance

  inline def strict[A]: CommutativeMonoid[A] =
    import Strict.given
    summonInline[DerivedCommutativeMonoid[A]].instance

  given [A](using inst: => ProductInstances[CommutativeMonoid |: Derived, A]): DerivedCommutativeMonoid[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: CommutativeMonoid[x], A](using @unused inst: ProductInstances[F, A])
      extends DerivedMonoid.Product[F, A],
        CommutativeMonoid[A]

  object Strict:
    given product[A: ProductInstancesOf[CommutativeMonoid]]: DerivedCommutativeMonoid[A] =
      new Product[CommutativeMonoid, A] {}
