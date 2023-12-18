package cats.derived

import cats.Order
import shapeless3.deriving.{Complete, K0}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Order[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Order instance
  * it is a sealed trait where all subclasses have an Order instance""")
type DerivedOrder[A] = Derived[Order[A]]
object DerivedOrder:
  type Or[A] = Derived.Or[Order[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Order[A] =
    import DerivedOrder.given
    summonInline[DerivedOrder[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Order[A] =
    import DerivedOrder.given
    import Strict.product
    summonInline[DerivedOrder[A]].instance

  given singleton[A <: Singleton: ValueOf]: DerivedOrder[A] =
    Order.allEqual

  given product[A](using inst: => K0.ProductInstances[Or, A]): DerivedOrder[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => K0.CoproductInstances[Or, A]): DerivedOrder[A] =
    given K0.CoproductInstances[Order, A] = inst.unify
    new Coproduct[Order, A] {}

  trait Product[T[x] <: Order[x], A](using inst: K0.ProductInstances[T, A]) extends Order[A]:
    def compare(x: A, y: A): Int =
      inst.foldLeft2(x, y)(0: Int):
        [t] =>
          (acc: Int, ord: T[t], t0: t, t1: t) =>
            val cmp = ord.compare(t0, t1)
            Complete(cmp != 0)(cmp)(acc)

  trait Coproduct[T[x] <: Order[x], A](using inst: K0.CoproductInstances[T, A]) extends Order[A]:
    def compare(x: A, y: A): Int =
      inst.fold2(x, y)((x: Int, y: Int) => x - y):
        [t] => (ord: T[t], t0: t, t1: t) => ord.compare(t0, t1)

  object Strict:
    given product[A](using => K0.ProductInstances[Order, A]): DerivedOrder[A] =
      new Product[Order, A] {}
