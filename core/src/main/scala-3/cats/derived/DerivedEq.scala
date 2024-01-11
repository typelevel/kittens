package cats.derived

import cats.Eq
import shapeless3.deriving.Complete
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Eq[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Eq instance
  * it is a sealed trait where all subclasses have an Eq instance""")
type DerivedEq[A] = Derived[Eq[A]]
object DerivedEq:
  type Or[A] = Derived.Or[Eq[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Eq[A] =
    import DerivedEq.given
    summonInline[DerivedEq[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Eq[A] =
    import Strict.given
    summonInline[DerivedEq[A]].instance

  given singleton[A <: Singleton: ValueOf]: DerivedEq[A] =
    Eq.allEqual

  given product[A](using inst: => ProductInstances[Or, A]): DerivedEq[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[Or, A]): DerivedEq[A] =
    given CoproductInstances[Eq, A] = inst.unify
    new Coproduct[Eq, A] {}

  trait Product[F[x] <: Eq[x], A](using inst: ProductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean):
      [t] => (acc: Boolean, eqt: F[t], x: t, y: t) => Complete(!eqt.eqv(x, y))(false)(true)

  trait Coproduct[F[x] <: Eq[x], A](using inst: CoproductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false):
      [t] => (eqt: F[t], x: t, y: t) => eqt.eqv(x, y)

  object Strict:
    export DerivedEq.coproduct
    given product[A: ProductInstancesOf[Eq]]: DerivedEq[A] =
      new Product[Eq, A] {}
