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

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[A]: PartialOrder[A] =
    import StackSafe.given
    summonInline[DerivedPartialOrder[A]].instance

  @unused
  given singleton[A <: Singleton: ValueOf]: DerivedPartialOrder[A] =
    Order.allEqual

  given product[A](using inst: => ProductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
    given CoproductInstances[PartialOrder, A] = inst.unify
    new Coproduct[PartialOrder, A] {}

  // ---- Default: fast direct recursion ----

  trait Product[T[x] <: PartialOrder[x], A](using inst: ProductInstances[T, A]) extends PartialOrder[A]:
    def partialCompare(x: A, y: A): Double =
      inst.foldLeft2(x, y)(0: Double): [t] =>
        (acc: Double, ord: T[t], t0: t, t1: t) =>
          val cmp = ord.partialCompare(t0, t1)
          Complete(cmp != 0)(cmp)(acc)

  trait Coproduct[T[x] <: PartialOrder[x], A](using inst: CoproductInstances[T, A]) extends PartialOrder[A]:
    def partialCompare(x: A, y: A): Double =
      inst.fold2(x, y)(Double.NaN: Double): [t] =>
        (ord: T[t], t0: t, t1: t) => ord.partialCompare(t0, t1)

  object Strict:
    export DerivedPartialOrder.coproduct
    given product[A: ProductInstancesOf[PartialOrder]]: DerivedPartialOrder[A] =
      new Product[PartialOrder, A] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    export DerivedPartialOrder.singleton
    given product[A](using inst: => ProductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
      given ProductInstances[PartialOrder, A] = inst.unify
      new SafeProduct[PartialOrder, A] {}

    given coproduct[A](using inst: => CoproductInstances[PartialOrder |: Derived, A]): DerivedPartialOrder[A] =
      given CoproductInstances[PartialOrder, A] = inst.unify
      new SafeCoproduct[PartialOrder, A] {}

  private[derived] trait Safe[A] extends PartialOrder[A]:
    private[derived] def safePartialCompare(x: A, y: A): Eval[Double]
    override def partialCompare(x: A, y: A): Double = safePartialCompare(x, y).value

  private[derived] def safePartialCompare[A](F: PartialOrder[A])(x: A, y: A): Eval[Double] =
    F.asInstanceOf[Matchable] match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safePartialCompare(x, y)
      case _ => Eval.later(F.partialCompare(x, y))

  trait SafeProduct[T[x] <: PartialOrder[x], A](using inst: ProductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safePartialCompare(x: A, y: A): Eval[Double] =
      inst.foldLeft2[Eval[Double]](x, y)(Eval.now(0.0)):
        [t] => (acc: Eval[Double], ord: T[t], t0: t, t1: t) =>
          val next = acc.flatMap: cmp =>
            if cmp != 0.0 then Eval.now(cmp) else DerivedPartialOrder.safePartialCompare(ord)(t0, t1)
          Complete(false)(next)(next)

  trait SafeCoproduct[T[x] <: PartialOrder[x], A](using inst: CoproductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safePartialCompare(x: A, y: A): Eval[Double] =
      Eval.defer(inst.fold2(x, y)(Eval.now(Double.NaN): Eval[Double]):
        [t] => (ord: T[t], t0: t, t1: t) => DerivedPartialOrder.safePartialCompare(ord)(t0, t1)
      )
