package cats.derived

import cats.Group
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Group for ${A}.
Make sure it is a case class where all fields form Group.""")
type DerivedGroup[A] = Derived[Group[A]]
object DerivedGroup:
  type Or[A] = Derived.Or[Group[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Group[A] =
    import DerivedGroup.given
    summonInline[DerivedGroup[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Group[A] =
    import Strict.given
    summonInline[DerivedGroup[A]].instance

  given product[A](using inst: => ProductInstances[Or, A]): DerivedGroup[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: Group[x], A: ProductInstancesOf[F]] extends DerivedMonoid.Product[F, A], Group[A]:
    final override def inverse(a: A): A = ProductInstances.map(a)([a] => (F: F[a], x: a) => F.inverse(x))

  object Strict:
    given product[A: ProductInstancesOf[Group]]: DerivedGroup[A] =
      new Product[Group, A] {}
