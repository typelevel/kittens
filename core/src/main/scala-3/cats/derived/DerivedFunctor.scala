package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Functor[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T
  * it is a nested type [x] =>> G[H[x]] where G: Functor and H: Functor
  * it is a nested type [x] =>> G[H[x]] where G: Contravariant and H: Contravariant
  * it is a generic case class where all fields have a Functor instance
  * it is a generic sealed trait where all subclasses have a Functor instance""")
type DerivedFunctor[F[_]] = Derived[Functor[F]]
object DerivedFunctor:
  type Or[F[_]] = Derived.Or[Functor[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose(G.unify)) with Functor[[x] =>> F[G[x]]]:
      export delegate.*

  given nested[F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedFunctor[F] =
    given K1.Instances[Functor, F] = inst.unify
    new Generic[Functor, F] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: K1.Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))
