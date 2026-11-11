package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Foldable for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Foldable and H: Foldable
  * generic case class where all fields form Foldable
  * generic sealed trait where all subclasses form Foldable
  * generic enum where all variants form Foldable""")
type DerivedFoldable[F[_]] = Derived[Foldable[F]]
object DerivedFoldable:
  inline def apply[F[_]]: Foldable[F] =
    import DerivedFoldable.given
    summonInline[DerivedFoldable[F]].instance

  inline def strict[F[_]]: Foldable[F] =
    import Strict.given
    summonInline[DerivedFoldable[F]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[F[_]]: Foldable[F] =
    import StackSafe.given
    summonInline[DerivedFoldable[F]].instance

  given [T]: DerivedFoldable[Const[T]] = new Foldable[Const[T]]:
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given nested[F[_], G[_]](using
      F: => (Foldable |: Derived)[F],
      G: => (Foldable |: Derived)[G]
  ): DerivedFoldable[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Foldable[F <<< G]:
      export delegate.*

  given [F[_]](using inst: ProductInstances[Foldable |: Derived, F]): DerivedFoldable[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Foldable |: Derived, F]): DerivedFoldable[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Foldable |: Derived, G[_]: Foldable |: Derived]: DerivedFoldable[[x] =>> F[G[x]]] = nested

  // ---- Default: fast direct recursion ----

  trait Product[T[f[_]] <: Foldable[f], F[_]](using inst: ProductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft(fa)(b)([f[_]] => (b: B, F: T[f], fa: f[A]) => F.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight(fa)(lb)([f[_]] => (F: T[f], fa: f[A], lb: Eval[B]) => Eval.defer(F.foldRight(fa, lb)(f)))

  trait Coproduct[T[f[_]] <: Foldable[f], F[_]](using inst: CoproductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.foldRight(fa, lb)(f)))

  object Strict:
    given product[F[_]: ProductInstancesOf[Foldable]]: DerivedFoldable[F] =
      new Product[Foldable, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Foldable |: Derived, F]): DerivedFoldable[F] =
      given CoproductInstances[Foldable, F] = inst.unify
      new Coproduct[Foldable, F] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    given product[F[_]](using inst: ProductInstances[Foldable |: Derived, F]): DerivedFoldable[F] =
      given ProductInstances[Foldable, F] = inst.unify
      new SafeProduct[Foldable, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Foldable |: Derived, F]): DerivedFoldable[F] =
      given CoproductInstances[Foldable, F] = inst.unify
      new SafeCoproduct[Foldable, F] {}

  private[derived] trait Safe[F[_]] extends Foldable[F]:
    private[derived] def safeFoldLeft[A, B](fa: F[A], b: Eval[B])(f: (B, A) => B): Eval[B]
    override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      safeFoldLeft(fa, Eval.now(b))(f).value

  private[derived] def safeFoldLeft[F[_], A, B](F: Foldable[F])(fa: F[A], b: Eval[B])(f: (B, A) => B): Eval[B] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeFoldLeft(fa, b)(f)
      case _ => b.map(F.foldLeft(fa, _)(f))

  trait SafeProduct[T[f[_]] <: Foldable[f], F[_]](using inst: ProductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeFoldLeft[A, B](fa: F[A], b: Eval[B])(f: (B, A) => B): Eval[B] =
      inst.foldLeft[A, Eval[B]](fa)(b):
        [f[_]] => (acc: Eval[B], F: T[f], fa: f[A]) =>
          DerivedFoldable.safeFoldLeft(F)(fa, acc)(f)

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight(fa)(lb)([f[_]] => (F: T[f], fa: f[A], lb: Eval[B]) => Eval.defer(F.foldRight(fa, lb)(f)))

  trait SafeCoproduct[T[f[_]] <: Foldable[f], F[_]](using inst: CoproductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeFoldLeft[A, B](fa: F[A], b: Eval[B])(f: (B, A) => B): Eval[B] =
      Eval.defer(inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) =>
        DerivedFoldable.safeFoldLeft(F)(fa, b)(f)
      ))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.foldRight(fa, lb)(f)))
