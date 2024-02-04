package cats.derived

import cats.kernel.Band
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Band[A] where A = ${A}.
Make sure that A is a case class where all fields have a Band instance.""")
type DerivedBand[A] = Derived[Band[A]]
object DerivedBand:
  type Or[A] = Derived.Or[Band[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Band[A] =
    import DerivedBand.given
    summonInline[DerivedBand[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Band[A] =
    import Strict.given
    summonInline[DerivedBand[A]].instance

  given product[A](using inst: => ProductInstances[Or, A]): DerivedBand[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: Band[x], A: ProductInstancesOf[F]] extends DerivedSemigroup.Product[F, A], Band[A]

  object Strict:
    given product[A: ProductInstancesOf[Band]]: DerivedBand[A] =
      new Product[Band, A] {}
