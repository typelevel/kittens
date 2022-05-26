package cats.derived

import cats.{PartialOrder, Show}
import shapeless3.deriving.{Complete, Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.compiletime.*
import scala.deriving.Mirror

@implicitNotFound("""Could not derive an instance of PartialOrder[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a PartialOrder instance
  * it is a sealed trait where all subclasses have a PartialOrder instance""")
type DerivedPartialOrder[A] = Derived[PartialOrder[A]]
object DerivedPartialOrder:
  type Or[A] = Derived.Or[PartialOrder[A]]

  inline def apply[A]: PartialOrder[A] =
    import DerivedPartialOrder.given
    summonInline[DerivedPartialOrder[A]].instance

  given product[A](using inst: => K0.ProductInstances[Or, A]): DerivedPartialOrder[A] =
    given K0.ProductInstances[PartialOrder, A] = inst.unify
    new Product[PartialOrder, A] {}

  given coproduct[A](using inst: => K0.CoproductInstances[Or, A]): DerivedPartialOrder[A] =
    given K0.CoproductInstances[PartialOrder, A] = inst.unify
    new Coproduct[PartialOrder, A] {}

  trait Product[T[x] <: PartialOrder[x], A](using inst: K0.ProductInstances[T, A]) extends PartialOrder[A]:

    def partialCompare(x: A, y: A): Double =
      inst.foldLeft2(x, y)(0: Double)(
        [t] =>
          (acc: Double, ord: T[t], t0: t, t1: t) => {
            val cmp = ord.partialCompare(t0, t1)
            Complete(cmp != 0)(cmp)(acc)
        }
      )

  trait Coproduct[T[x] <: PartialOrder[x], A](using inst: K0.CoproductInstances[T, A])
      extends PartialOrder[A]:

    def partialCompare(x: A, y: A): Double =
      inst.fold2(x, y)(Double.NaN: Double)(
        [t] => (ord: T[t], t0: t, t1: t) => ord.partialCompare(t0, t1)
      )
