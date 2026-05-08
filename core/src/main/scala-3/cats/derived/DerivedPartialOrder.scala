package cats.derived

import cats.{Eval, Order, PartialOrder}
import shapeless3.deriving.{Complete, Derived}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive PartialOrder for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form PartialOrder
  * sealed trait where all subclasses form PartialOrder
  * enum where all variants form PartialOrder""")
type DerivedPartialOrder[A] = Derived[PartialOrder[A]]
object DerivedPartialOrder:
  inline def apply[A]: PartialOrder[A] =
    import DerivedPartialOrder.given
    summonInline[DerivedPartialOrder[A]].instance

  inline def strict[A]: PartialOrder[A] =
    import Strict.given
    summonInline[DerivedPartialOrder[A]].instance

  @unused
  given singleton[A <: Singleton: ValueOf]: DerivedPartialOrder[A] =
    Order.allEqual

  given product[A](using inst: => ProductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
    given CoproductInstances[PartialOrder, A] = inst.unify
    new Coproduct[PartialOrder, A] {}

  private[derived] trait Safe[A] extends PartialOrder[A]:
    private[derived] def safePartialCompare(x: A, y: A): Eval[Double]
    override def partialCompare(x: A, y: A): Double = safePartialCompare(x, y).value

  private[derived] def safePartialCompare[A](F: PartialOrder[A])(x: A, y: A): Eval[Double] =
    F.asInstanceOf[Matchable] match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safePartialCompare(x, y)
      case _ => Eval.later(F.partialCompare(x, y))

  trait Product[T[x] <: PartialOrder[x], A](using inst: ProductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safePartialCompare(x: A, y: A): Eval[Double] =
      inst.foldLeft2[Eval[Double]](x, y)(Eval.now(0.0)):
        [t] => (acc: Eval[Double], ord: T[t], t0: t, t1: t) =>
          val next = acc.flatMap: cmp =>
            if cmp != 0.0 then Eval.now(cmp) else DerivedPartialOrder.safePartialCompare(ord)(t0, t1)
          Complete(false)(next)(next)

  trait Coproduct[T[x] <: PartialOrder[x], A](using inst: CoproductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safePartialCompare(x: A, y: A): Eval[Double] =
      Eval.defer(inst.fold2(x, y)(Eval.now(Double.NaN): Eval[Double]):
        [t] => (ord: T[t], t0: t, t1: t) => DerivedPartialOrder.safePartialCompare(ord)(t0, t1)
      )

  object Strict:
    export DerivedPartialOrder.coproduct
    given product[A: ProductInstancesOf[PartialOrder]]: DerivedPartialOrder[A] =
      new Product[PartialOrder, A] {}
