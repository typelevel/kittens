package cats.derived

import cats.Order
import shapeless3.deriving.{K0, Complete}

object order extends OrderDerivation

trait ProductOrder[T[x] <: Order[x], A](using inst: K0.ProductInstances[T, A])
    extends Order[A]:

  def compare(x: A, y: A): Int =
    inst.foldLeft2(x, y)(0: Int)(
      [t] => (acc: Int, ord: T[t], t0: t, t1: t) => {
        val cmp = ord.compare(t0, t1)
        Complete(cmp != 0)(cmp)(acc)
      }
    )

trait CoproductOrder[T[x] <: Order[x], A](using inst: K0.CoproductInstances[T, A])
    extends Order[A]:

  def compare(x: A, y: A): Int =
    inst.fold2(x, y)((x: Int, y: Int) => x - y)(
      [t] => (ord: T[t], t0: t, t1: t) => ord.compare(t0, t1)
    )

trait OrderDerivation:
  extension (F: Order.type)
    inline def derived[A](using gen: K0.Generic[A]): Order[A] =
      gen.derive(productOrder, coproductOrder)

  given productOrder[A](using K0.ProductInstances[Order, A]): Order[A] =
    new ProductOrder[Order, A]{}

  given coproductOrder[A](using K0.CoproductInstances[Order, A]): Order[A] =
    new CoproductOrder[Order, A]{}
