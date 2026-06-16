package cats.derived

import cats.{Contravariant, Eval, Functor}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Functor for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Functor and H: Functor
  * nested type [x] =>> G[H[x]] where G: Contravariant and H: Contravariant
  * generic case class where all fields form Functor
  * generic sealed trait where all subclasses form Functor
  * generic enum where all variants form Functor""")
type DerivedFunctor[F[_]] = Derived[Functor[F]]
object DerivedFunctor:
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  inline def strict[F[_]]: Functor[F] =
    import Strict.given
    summonInline[DerivedFunctor[F]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[F[_]]: Functor[F] =
    import StackSafe.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using
      F: => (Functor |: Derived)[F],
      G: => (Functor |: Derived)[G]
  ): DerivedFunctor[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Functor[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      F: (Contravariant |: Derived)[F],
      G: (Contravariant |: Derived)[G]
  ): DerivedFunctor[F <<< G] =
    F.unify.compose(using G.unify)

  given [F[_]](using inst: ProductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: (Functor |: Derived)[F],
      G: (Functor |: Derived)[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: (Contravariant |: Derived)[F],
      G: (Contravariant |: Derived)[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  // ---- Default: fast direct recursion ----

  private def generic[F[_]: InstancesOf[Functor]]: DerivedFunctor[F] =
    new Generic[Functor, F] {}

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))

  object Strict:
    given product[F[_]: ProductInstancesOf[Functor]]: DerivedFunctor[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
      generic(using inst.unify)

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    given product[F[_]](using inst: ProductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
      given ProductInstances[Functor, F] = inst.unify
      new SafeProduct[Functor, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
      given CoproductInstances[Functor, F] = inst.unify
      new SafeCoproduct[Functor, F] {}

  private[derived] trait Safe[F[_]] extends Functor[F]:
    private[derived] def safeMap[A, B](fa: F[A])(f: A => B): Eval[F[B]]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = safeMap(fa)(f).value

  private[derived] def safeMap[F[_], A, B](F: Functor[F])(fa: F[A])(f: A => B): Eval[F[B]] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeMap(fa)(f)
      case _ => Eval.later(F.map(fa)(f))

  trait SafeProduct[T[f[_]] <: Functor[f], F[_]](using inst: ProductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeMap[A, B](fa: F[A])(f: A => B): Eval[F[B]] =
      val pure = [a] => (x: a) => Eval.now(x)
      val mp = [a, b] => (ea: Eval[a], g: a => b) => ea.map(g)
      val ap = [a, b] => (ef: Eval[a => b], ea: Eval[a]) => ef.flatMap(g => ea.map(g))
      inst.traverse[A, Eval, B](fa)(mp)(pure)(ap)(
        [f[_]] => (F: T[f], fa: f[A]) => DerivedFunctor.safeMap(F)(fa)(f)
      )

  trait SafeCoproduct[T[f[_]] <: Functor[f], F[_]](using inst: CoproductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeMap[A, B](fa: F[A])(f: A => B): Eval[F[B]] =
      Eval.defer(inst.fold(fa)(
        [f[a] <: F[a]] => (F: T[f], fa: f[A]) =>
          DerivedFunctor.safeMap(F)(fa)(f).asInstanceOf[Eval[F[B]]]
      ))
