package cats.derived

import cats.Eq
import shapeless3.deriving.{Complete, Derived}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Eq for ${A}.
Make sure that A satisfies one of the following conditions:
  * case class where all fields form Eq
  * sealed trait where all subclasses form Eq
  * enum where all variants form Eq""")
type DerivedEq[A] = Derived[Eq[A]]
object DerivedEq:
  inline def apply[A]: Eq[A] =
    import DerivedEq.given
    summonInline[DerivedEq[A]].instance

  inline def strict[A]: Eq[A] =
    import Strict.given
    summonInline[DerivedEq[A]].instance

  @unused
  given singleton[A <: Singleton: ValueOf]: DerivedEq[A] =
    Eq.allEqual

  given product[A](using inst: => ProductInstances[Eq |: Derived, A]): DerivedEq[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[Eq |: Derived, A]): DerivedEq[A] =
    given CoproductInstances[Eq, A] = inst.unify
    new Coproduct[Eq, A] {}

  trait Product[F[x] <: Eq[x], A](using inst: ProductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean): [t] =>
      (acc: Boolean, eqt: F[t], x: t, y: t) => Complete(!eqt.eqv(x, y))(false)(acc)

  trait Coproduct[F[x] <: Eq[x], A](using inst: CoproductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false): [t] =>
      (eqt: F[t], x: t, y: t) => eqt.eqv(x, y)

  object Strict:
    export DerivedEq.coproduct
    given product[A: ProductInstancesOf[Eq]]: DerivedEq[A] =
      new Product[Eq, A] {}
