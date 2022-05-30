package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, K1}

import scala.annotation.implicitNotFound
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
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    given Contravariant[G] = G.unify
    F.unify.compose[G]

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedFunctor[F] =
    given K1.Instances[Functor, F] = inst.unify
    new Generic[Functor, F] {}

  trait Generic[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A])([f[_]] => (tf: T[f], fa: f[A]) => tf.map(fa)(f))
