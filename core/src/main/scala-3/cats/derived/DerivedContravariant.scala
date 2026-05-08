package cats.derived

import cats.{Contravariant, Eval, Functor}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Contravariant for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Functor and H: Contravariant
  * generic case class where all fields form Contravariant
  * generic sealed trait where all subclasses form Contravariant
  * generic enum where all variants form Contravariant""")
type DerivedContravariant[F[_]] = Derived[Contravariant[F]]
object DerivedContravariant:
  inline def apply[F[_]]: Contravariant[F] =
    import DerivedContravariant.given
    summonInline[DerivedContravariant[F]].instance

  inline def strict[F[_]]: Contravariant[F] =
    import Strict.given
    summonInline[DerivedContravariant[F]].instance

  given [T]: DerivedContravariant[Const[T]] = new Contravariant[Const[T]]:
    def contramap[A, B](fa: T)(f: B => A): T = fa

  given nested[F[_], G[_]](using
      F: (Functor |: Derived)[F],
      G: => (Contravariant |: Derived)[G]
  ): DerivedContravariant[F <<< G] =
    new Lazy(() => F.unify.composeContravariant(using G.unify)) with Contravariant[F <<< G]:
      export delegate.*

  given [F[_]](using inst: ProductInstances[Contravariant |: Derived, F]): DerivedContravariant[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Contravariant |: Derived, F]): DerivedContravariant[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Functor |: Derived, G[_]: Contravariant |: Derived]: DerivedContravariant[[x] =>> F[G[x]]] =
    nested

  private[derived] trait Safe[F[_]] extends Contravariant[F]:
    private[derived] def safeContramap[A, B](fa: F[A])(f: B => A): Eval[F[B]]
    override def contramap[A, B](fa: F[A])(f: B => A): F[B] = safeContramap(fa)(f).value

  private[derived] def safeContramap[F[_], A, B](F: Contravariant[F])(fa: F[A])(f: B => A): Eval[F[B]] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeContramap(fa)(f)
      case _ => Eval.later(F.contramap(fa)(f))

  trait Generic[T[f[_]] <: Contravariant[f], F[_]](using inst: Instances[T, F]) extends Contravariant[F]:
    final override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
      inst.map(fa)([f[_]] => (T: T[f], fa: f[A]) => T.contramap(fa)(f))

  trait Product[T[f[_]] <: Contravariant[f], F[_]](using inst: ProductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeContramap[A, B](fa: F[A])(f: B => A): Eval[F[B]] =
      val pure = [a] => (x: a) => Eval.now(x)
      val mp = [a, b] => (ea: Eval[a], g: a => b) => ea.map(g)
      val ap = [a, b] => (ef: Eval[a => b], ea: Eval[a]) => ef.flatMap(g => ea.map(g))
      inst.traverse[A, Eval, B](fa)(mp)(pure)(ap)(
        [f[_]] => (F: T[f], fa: f[A]) => DerivedContravariant.safeContramap(F)(fa)(f)
      )

  trait Coproduct[T[f[_]] <: Contravariant[f], F[_]](using inst: CoproductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeContramap[A, B](fa: F[A])(f: B => A): Eval[F[B]] =
      Eval.defer(inst.fold(fa)(
        [f[a] <: F[a]] => (F: T[f], fa: f[A]) =>
          DerivedContravariant.safeContramap(F)(fa)(f).asInstanceOf[Eval[F[B]]]
      ))

  object Strict:
    given product[F[_]: ProductInstancesOf[Contravariant]]: DerivedContravariant[F] =
      new Product[Contravariant, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Contravariant |: Derived, F]): DerivedContravariant[F] =
      given CoproductInstances[Contravariant, F] = inst.unify
      new Coproduct[Contravariant, F] {}
