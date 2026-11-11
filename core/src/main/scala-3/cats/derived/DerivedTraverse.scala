package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Traverse for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Traverse and H: Traverse
  * generic case class where all fields form Traverse
  * generic sealed trait where all subclasses form Traverse
  * generic enum where all variants form Traverse""")
type DerivedTraverse[F[_]] = Derived[Traverse[F]]
object DerivedTraverse:
  inline def apply[F[_]]: Traverse[F] =
    import DerivedTraverse.given
    summonInline[DerivedTraverse[F]].instance

  inline def strict[F[_]]: Traverse[F] =
    import Strict.given
    summonInline[DerivedTraverse[F]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[F[_]]: Traverse[F] =
    import StackSafe.given
    summonInline[DerivedTraverse[F]].instance

  given [T]: DerivedTraverse[Const[T]] = new Traverse[Const[T]]:
    override def map[A, B](fa: T)(f: A => B): T = fa
    override def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    override def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    override def traverse[G[_], A, B](fa: T)(f: A => G[B])(using G: Applicative[G]): G[T] = G.pure(fa)

  given nested[F[_], G[_]](using
      F: => (Traverse |: Derived)[F],
      G: => (Traverse |: Derived)[G]
  ): DerivedTraverse[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Traverse[F <<< G]:
      export delegate.*

  given [F[_]](using inst: ProductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Traverse |: Derived, G[_]: Traverse |: Derived]: DerivedTraverse[[x] =>> F[G[x]]] = nested

  // ---- Default: fast direct recursion ----

  trait Product[T[f[_]] <: Traverse[f], F[_]](using inst: ProductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Product[T, F]:

    final override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      val pure = [a] => (x: a) => G.pure(x)
      val map = [a, b] => (ga: G[a], f: a => b) => G.map(ga)(f)
      val ap = [a, b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga)
      inst.traverse[A, G, B](fa)(map)(pure)(ap)([f[_]] => (F: T[f], fa: f[A]) => F.traverse(fa)(f))

  trait Coproduct[T[f[_]] <: Traverse[f], F[_]](using inst: CoproductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Coproduct[T, F]:

    final override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      inst.fold(fa)([f[a] <: F[a]] => (F: T[f], fa: f[A]) => G.widen[f[B], F[B]](F.traverse(fa)(f)))

  object Strict:
    given product[F[_]: ProductInstancesOf[Traverse]]: DerivedTraverse[F] =
      new Traverse[F] with Product[Traverse, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
      given CoproductInstances[Traverse, F] = inst.unify
      new Traverse[F] with Coproduct[Traverse, F] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    given product[F[_]](using inst: ProductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
      given ProductInstances[Traverse, F] = inst.unify
      new Traverse[F] with SafeProduct[Traverse, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
      given CoproductInstances[Traverse, F] = inst.unify
      new Traverse[F] with SafeCoproduct[Traverse, F] {}

  private[derived] trait Safe[F[_]] extends Traverse[F]:
    private[derived] def safeTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): Eval[G[F[B]]]
    override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      safeTraverse(fa)(f).value

  private[derived] def safeTraverse[F[_], G[_], A, B](
      F: Traverse[F]
  )(fa: F[A])(f: A => G[B])(using G: Applicative[G]): Eval[G[F[B]]] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeTraverse(fa)(f)
      case _ => Eval.later(F.traverse(fa)(f))

  trait SafeProduct[T[f[_]] <: Traverse[f], F[_]](using inst: ProductInstances[T, F])
      extends Safe[F],
        DerivedFunctor.SafeProduct[T, F],
        DerivedFoldable.SafeProduct[T, F]:

    private[derived] final override def safeTraverse[G[_], A, B](
        fa: F[A]
    )(f: A => G[B])(using G: Applicative[G]): Eval[G[F[B]]] =
      val pure = [a] => (x: a) => Eval.now(G.pure(x))
      val mp = [a, b] => (ega: Eval[G[a]], h: a => b) => ega.map(ga => G.map(ga)(h))
      val ap = [a, b] => (egf: Eval[G[a => b]], ega: Eval[G[a]]) =>
        egf.flatMap(gf => ega.map(ga => G.ap(gf)(ga)))
      inst.traverse[A, [x] =>> Eval[G[x]], B](fa)(mp)(pure)(ap)(
        [f[_]] => (F: T[f], fa: f[A]) => DerivedTraverse.safeTraverse(F)(fa)(f)
      )

  trait SafeCoproduct[T[f[_]] <: Traverse[f], F[_]](using inst: CoproductInstances[T, F])
      extends Safe[F],
        DerivedFunctor.SafeCoproduct[T, F],
        DerivedFoldable.SafeCoproduct[T, F]:

    private[derived] final override def safeTraverse[G[_], A, B](
        fa: F[A]
    )(f: A => G[B])(using G: Applicative[G]): Eval[G[F[B]]] =
      Eval.defer(inst.fold(fa):
        [f[a] <: F[a]] => (F: T[f], fa: f[A]) =>
          DerivedTraverse.safeTraverse(F)(fa)(f).map(g => G.widen[f[B], F[B]](g)).asInstanceOf[Eval[G[F[B]]]]
      )
