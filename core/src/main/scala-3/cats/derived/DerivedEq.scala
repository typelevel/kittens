package cats.derived

import cats.Eq
import shapeless3.deriving.{Complete, K0}

import scala.compiletime.*

type DerivedEq[A] = Derived[Eq[A]]
object DerivedEq:
  type Or[A] = Derived.Or[Eq[A]]
  inline def apply[A]: Eq[A] =
    import DerivedEq.given
    summonInline[DerivedEq[A]].instance

  given [A](using inst: K0.ProductInstances[Or, A]): DerivedEq[A] =
    given K0.ProductInstances[Eq, A] = inst.unify
    new Product[Eq, A] {}

  given [A](using inst: => K0.CoproductInstances[Or, A]): DerivedEq[A] =
    given K0.CoproductInstances[Eq, A] = inst.unify
    new Coproduct[Eq, A] {}

  trait Product[F[x] <: Eq[x], A](using inst: K0.ProductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
      [t] => (acc: Boolean, eqt: F[t], x: t, y: t) => Complete(!eqt.eqv(x, y))(false)(true)
    )

  trait Coproduct[F[x] <: Eq[x], A](using inst: K0.CoproductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
      [t] => (eqt: F[t], x: t, y: t) => eqt.eqv(x, y)
    )
