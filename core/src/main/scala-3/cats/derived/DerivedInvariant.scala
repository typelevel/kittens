package cats.derived

import cats.{Eval, Invariant}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Invariant for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] => T
  * nested type [x] => G[H[x]] where G: Invariant and H: Invariant
  * generic case class where all fields form Invariant
  * generic sealed trait where all subclasses form Invariant
  * generic enum where all variants form Invariant""")
type DerivedInvariant[F[_]] = Derived[Invariant[F]]
object DerivedInvariant:
  inline def apply[F[_]]: Invariant[F] =
    import DerivedInvariant.given
    summonInline[DerivedInvariant[F]].instance

  inline def strict[F[_]]: Invariant[F] =
    import Strict.given
    summonInline[DerivedInvariant[F]].instance

  given [T]: DerivedInvariant[Const[T]] = new Invariant[Const[T]]:
    def imap[A, B](fa: T)(f: A => B)(g: B => A): T = fa

  given nested[F[_], G[_]](using
      F: => (Invariant |: Derived)[F],
      G: => (Invariant |: Derived)[G]
  ): DerivedInvariant[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Invariant[F <<< G]:
      export delegate.*

  given [F[_]](using inst: ProductInstances[Invariant |: Derived, F]): DerivedInvariant[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Invariant |: Derived, F]): DerivedInvariant[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Invariant |: Derived, G[_]: Invariant |: Derived]: DerivedInvariant[[x] =>> F[G[x]]] =
    nested

  private[derived] trait Safe[F[_]] extends Invariant[F]:
    private[derived] def safeImap[A, B](fa: F[A])(f: A => B)(g: B => A): Eval[F[B]]
    override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = safeImap(fa)(f)(g).value

  private[derived] def safeImap[F[_], A, B](F: Invariant[F])(fa: F[A])(f: A => B)(g: B => A): Eval[F[B]] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeImap(fa)(f)(g)
      case _ => Eval.later(F.imap(fa)(f)(g))

  trait Generic[T[f[_]] <: Invariant[f], F[_]](using inst: Instances[T, F]) extends Invariant[F]:
    final override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.imap(fa)(f)(g))

  trait Product[T[f[_]] <: Invariant[f], F[_]](using inst: ProductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeImap[A, B](fa: F[A])(f: A => B)(g: B => A): Eval[F[B]] =
      val pure = [a] => (x: a) => Eval.now(x)
      val mp = [a, b] => (ea: Eval[a], h: a => b) => ea.map(h)
      val ap = [a, b] => (ef: Eval[a => b], ea: Eval[a]) => ef.flatMap(h => ea.map(h))
      inst.traverse[A, Eval, B](fa)(mp)(pure)(ap)(
        [f[_]] => (F: T[f], fa: f[A]) => DerivedInvariant.safeImap(F)(fa)(f)(g)
      )

  trait Coproduct[T[f[_]] <: Invariant[f], F[_]](using inst: CoproductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeImap[A, B](fa: F[A])(f: A => B)(g: B => A): Eval[F[B]] =
      Eval.defer(inst.fold(fa)(
        [f[a] <: F[a]] => (F: T[f], fa: f[A]) =>
          DerivedInvariant.safeImap(F)(fa)(f)(g).asInstanceOf[Eval[F[B]]]
      ))

  object Strict:
    given product[F[_]: ProductInstancesOf[Invariant]]: DerivedInvariant[F] =
      new Product[Invariant, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Invariant |: Derived, F]): DerivedInvariant[F] =
      given CoproductInstances[Invariant, F] = inst.unify
      new Coproduct[Invariant, F] {}
