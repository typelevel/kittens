package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}
import scala.compiletime.*
import cats.Contravariant

type DerivedFunctor[F[_]] = Derived[Functor[F]]
object DerivedFunctor:
  type Or[F[_]] = Derived.Or[Functor[F]]
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    F.unify `compose` G.unify

  given [F[_], G[_]](using F: Contravariant[F], G: Contravariant[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    F `compose` G

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedFunctor[F] =
    new Generic(using inst.unify) {}

  trait Generic[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends Functor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.map(fa)(f)
      }
