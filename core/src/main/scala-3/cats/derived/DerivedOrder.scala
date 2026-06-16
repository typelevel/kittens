package cats.derived

import cats.{Eval, Order}
import shapeless3.deriving.{Complete, Derived}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Order for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Order
  * sealed trait where all subclasses form Order
  * enum where all variants form Order""")
type DerivedOrder[A] = Derived[Order[A]]
object DerivedOrder:
  inline def apply[A]: Order[A] =
    import DerivedOrder.given
    summonInline[DerivedOrder[A]].instance

  inline def strict[A]: Order[A] =
    import Strict.given
    summonInline[DerivedOrder[A]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[A]: Order[A] =
    import StackSafe.given
    summonInline[DerivedOrder[A]].instance

  @unused
  given singleton[A <: Singleton: ValueOf]: DerivedOrder[A] =
    Order.allEqual

  given product[A](using inst: => ProductInstances[Order |: Derived, A]): DerivedOrder[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[Order |: Derived, A]): DerivedOrder[A] =
    given CoproductInstances[Order, A] = inst.unify
    new Coproduct[Order, A] {}

  // ---- Default: fast direct recursion ----

  trait Product[T[x] <: Order[x], A](using inst: ProductInstances[T, A]) extends Order[A]:
    def compare(x: A, y: A): Int =
      inst.foldLeft2(x, y)(0: Int):
        [t] =>
          (acc: Int, ord: T[t], t0: t, t1: t) =>
            val cmp = ord.compare(t0, t1)
            Complete(cmp != 0)(cmp)(acc)

  trait Coproduct[T[x] <: Order[x], A](using inst: CoproductInstances[T, A]) extends Order[A]:
    def compare(x: A, y: A): Int =
      inst.fold2(x, y)((x: Int, y: Int) => x - y):
        [t] => (ord: T[t], t0: t, t1: t) => ord.compare(t0, t1)

  object Strict:
    export DerivedOrder.coproduct
    given product[A: ProductInstancesOf[Order]]: DerivedOrder[A] =
      new Product[Order, A] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    export DerivedOrder.singleton
    given product[A](using inst: => ProductInstances[Order |: Derived, A]): DerivedOrder[A] =
      given ProductInstances[Order, A] = inst.unify
      new SafeProduct[Order, A] {}

    given coproduct[A](using inst: => CoproductInstances[Order |: Derived, A]): DerivedOrder[A] =
      given CoproductInstances[Order, A] = inst.unify
      new SafeCoproduct[Order, A] {}

  private[derived] trait Safe[A] extends Order[A]:
    private[derived] def safeCompare(x: A, y: A): Eval[Int]
    override def compare(x: A, y: A): Int = safeCompare(x, y).value

  private[derived] def safeCompare[A](F: Order[A])(x: A, y: A): Eval[Int] =
    F.asInstanceOf[Matchable] match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safeCompare(x, y)
      case _ => Eval.later(F.compare(x, y))

  trait SafeProduct[T[x] <: Order[x], A](using inst: ProductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safeCompare(x: A, y: A): Eval[Int] =
      inst.foldLeft2[Eval[Int]](x, y)(Eval.now(0)):
        [t] => (acc: Eval[Int], ord: T[t], t0: t, t1: t) =>
          val next = acc.flatMap: cmp =>
            if cmp != 0 then Eval.now(cmp) else DerivedOrder.safeCompare(ord)(t0, t1)
          Complete(false)(next)(next)

  trait SafeCoproduct[T[x] <: Order[x], A](using inst: CoproductInstances[T, A]) extends Safe[A]:
    private[derived] final override def safeCompare(x: A, y: A): Eval[Int] =
      Eval.defer(inst.fold2(x, y)((x: Int, y: Int) => Eval.now(x - y)):
        [t] => (ord: T[t], t0: t, t1: t) => DerivedOrder.safeCompare(ord)(t0, t1)
      )
