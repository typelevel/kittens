package cats.derived

import cats.{Eq, Eval}
import shapeless3.deriving.{Complete, Derived}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Eq for ${A}.
Make sure that A satisfies one of the following conditions:
  * case class where all fields form Eq
  * sealed trait where all subclasses form Eq
  * enum where all variants form Eq""")
type DerivedEq[A] = Derived[Eq[A]]
object DerivedEq:
  inline def apply[A]: Eq[A] =
    import DerivedEq.given
    summonInline[DerivedEq[A]].instance

  inline def strict[A]: Eq[A] =
    import Strict.given
    summonInline[DerivedEq[A]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[A]: Eq[A] =
    import StackSafe.given
    summonInline[DerivedEq[A]].instance

  @unused
  given singleton[A <: Singleton: ValueOf]: DerivedEq[A] =
    Eq.allEqual

  given product[A](using inst: => ProductInstances[Eq |: Derived, A]): DerivedEq[A] =
    Strict.product(using inst.unify)

  given coproduct[A](using inst: => CoproductInstances[Eq |: Derived, A]): DerivedEq[A] =
    given CoproductInstances[Eq, A] = inst.unify
    new Coproduct[Eq, A] {}

  // ---- Default: fast direct recursion ----

  trait Product[F[x] <: Eq[x], A](using inst: ProductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean): [t] =>
      (acc: Boolean, eqt: F[t], x: t, y: t) => Complete(!eqt.eqv(x, y))(false)(acc)

  trait Coproduct[F[x] <: Eq[x], A](using inst: CoproductInstances[F, A]) extends Eq[A]:
    final override def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false): [t] =>
      (eqt: F[t], x: t, y: t) => eqt.eqv(x, y)

  object Strict:
    export DerivedEq.coproduct
    given product[A: ProductInstancesOf[Eq]]: DerivedEq[A] =
      new Product[Eq, A] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    given product[A](using inst: => ProductInstances[Eq |: Derived, A]): DerivedEq[A] =
      given ProductInstances[Eq, A] = inst.unify
      new SafeProduct[Eq, A] {}

    given coproduct[A](using inst: => CoproductInstances[Eq |: Derived, A]): DerivedEq[A] =
      given CoproductInstances[Eq, A] = inst.unify
      new SafeCoproduct[Eq, A] {}

  private[derived] trait Safe[A] extends Eq[A]:
    private[derived] def safeEqv(x: A, y: A): Eval[Boolean]
    override def eqv(x: A, y: A): Boolean = safeEqv(x, y).value

  private[derived] def safeEqv[A](F: Eq[A])(x: A, y: A): Eval[Boolean] =
    F.asInstanceOf[Matchable] match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safeEqv(x, y)
      case _ => Eval.later(F.eqv(x, y))

  trait SafeProduct[F[x] <: Eq[x], A](using inst: ProductInstances[F, A]) extends Safe[A]:
    private[derived] final override def safeEqv(x: A, y: A): Eval[Boolean] =
      inst.foldLeft2[Eval[Boolean]](x, y)(Eval.now(true)):
        [t] => (acc: Eval[Boolean], eqt: F[t], xt: t, yt: t) =>
          val next = acc.flatMap: b =>
            if !b then Eval.now(false) else DerivedEq.safeEqv(eqt)(xt, yt)
          Complete(false)(next)(next)

  trait SafeCoproduct[F[x] <: Eq[x], A](using inst: CoproductInstances[F, A]) extends Safe[A]:
    private[derived] final override def safeEqv(x: A, y: A): Eval[Boolean] =
      Eval.defer(inst.fold2(x, y)(Eval.now(false): Eval[Boolean]):
        [t] => (eqt: F[t], xt: t, yt: t) => DerivedEq.safeEqv(eqt)(xt, yt)
      )
