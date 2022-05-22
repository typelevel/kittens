package cats.derived

import cats.{Order, Show}
import shapeless3.deriving.{Complete, Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.compiletime.*
import scala.deriving.Mirror

@implicitNotFound("""Could not derive an instance of Order[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Order instance
  * it is a sealed trait where all subclasses have an Order instance""")
type DerivedOrder[A] = Derived[Order[A]]
object DerivedOrder:
  type Or[A] = Derived.Or[Order[A]]

  inline def apply[A]: Order[A] =
    import DerivedOrder.given
    summonInline[DerivedOrder[A]].instance

  given product[A](using inst: => K0.ProductInstances[Or, A]): DerivedOrder[A] =
    given K0.ProductInstances[Order, A] = inst.unify
    new Product[Order, A] {}

  given coproduct[A](using inst: => K0.CoproductInstances[Or, A]): DerivedOrder[A] =
    given K0.CoproductInstances[Order, A] = inst.unify
    new Coproduct[Order, A] {}

  trait Product[T[x] <: Order[x], A](using inst: K0.ProductInstances[T, A]) extends Order[A]:

    def compare(x: A, y: A): Int =
      inst.foldLeft2(x, y)(0: Int)(
        [t] =>
          (acc: Int, ord: T[t], t0: t, t1: t) => {
            val cmp = ord.compare(t0, t1)
            Complete(cmp != 0)(cmp)(acc)
        }
      )

  trait Coproduct[T[x] <: Order[x], A](using inst: K0.CoproductInstances[T, A]) extends Order[A]:

    def compare(x: A, y: A): Int =
      inst.fold2(x, y)((x: Int, y: Int) => x - y)(
        [t] => (ord: T[t], t0: t, t1: t) => ord.compare(t0, t1)
      )
