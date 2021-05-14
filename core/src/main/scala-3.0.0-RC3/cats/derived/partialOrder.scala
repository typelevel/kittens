package cats.derived

import cats.PartialOrder
import shapeless3.deriving.{K0, Complete}

object partialOrder extends PartialOrderDerivation

trait ProductPartialOrder[T[x] <: PartialOrder[x], A](using inst: K0.ProductInstances[T, A])
    extends PartialOrder[A]:

  def partialCompare(x: A, y: A): Double =
    inst.foldLeft2(x, y)(0: Double)(
      [t] => (acc: Double, ord: T[t], t0: t, t1: t) => {
        val cmp = ord.partialCompare(t0, t1)
        Complete(cmp != 0)(cmp)(acc)
      }
    )

trait CoproductPartialOrder[T[x] <: PartialOrder[x], A](using inst: K0.CoproductInstances[T, A])
    extends PartialOrder[A]:

  def partialCompare(x: A, y: A): Double =
    //TODO is this correct? I _think_ it must be to be consistent with Order
    inst.fold2(x, y)((x: Int, y: Int) => (x - y).toDouble)(
      [t] => (ord: T[t], t0: t, t1: t) => ord.partialCompare(t0, t1)
    )

trait PartialOrderDerivation:
  extension (F: PartialOrder.type)
    inline def derived[A](using gen: K0.Generic[A]): PartialOrder[A] =
      gen.derive(productPartialOrder, coproductPartialOrder)

  given productPartialOrder[A](using K0.ProductInstances[PartialOrder, A]): PartialOrder[A] =
    new ProductPartialOrder[PartialOrder, A]{}

  given coproductPartialOrder[A](using K0.CoproductInstances[PartialOrder, A]): PartialOrder[A] =
    new CoproductPartialOrder[PartialOrder, A]{}
