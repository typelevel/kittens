package cats.derived

import cats.Eq
import shapeless3.deriving.{K0, Complete}

object eq extends EqDerivation

trait ProductEq[T[x] <: Eq[x], A](using inst: K0.ProductInstances[T, A]) extends Eq[A]:

  def eqv(x: A, y: A): Boolean =
    inst.foldLeft2(x, y)(true: Boolean)(
      [t] => (acc: Boolean, eqt: T[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
    )

trait CoproductEq[T[x] <: Eq[x], A](using inst: K0.CoproductInstances[T, A]) extends Eq[A]:

  def eqv(x: A, y: A): Boolean =
    inst.fold2(x, y)(false)(
      [t] => (eqt: T[t], t0: t, t1: t) => eqt.eqv(t0, t1)
    )


trait EqDerivation:
  extension (F: Eq.type)
    inline def derived[A](using gen: K0.Generic[A]): Eq[A] =
      gen.derive(productEq, coproductEq)

  given productEq[A](using K0.ProductInstances[Eq, A]): Eq[A] =
    new ProductEq[Eq, A]{}

  given coproductEq[A](using K0.CoproductInstances[Eq, A]): Eq[A] =
    new CoproductEq[Eq, A]{}
