package cats.derived

import cats.Group
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Group[A] where A = ${A}.
Make sure that A is a case class where all fields have a Group instance.""")
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

  given [A](using inst: => ProductInstances[Or, A]): DerivedGroup[A] =
    Strict.product(using inst.unify)

  trait Product[F[x] <: Group[x], A](using inst: ProductInstances[F, A]) extends DerivedMonoid.Product[F, A], Group[A]:
    override def inverse(a: A): A = inst.map(a)([a] => (F: F[a], x: a) => F.inverse(x))

  object Strict:
    given product[A: ProductInstancesOf[Group]]: DerivedGroup[A] =
      new Product[Group, A] {}
