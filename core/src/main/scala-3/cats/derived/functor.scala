package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}
import scala.annotation.threadUnsafe

object functor extends FunctorDerivation, Instances

trait DerivedFunctor[F[_]] extends Functor[F]
object DerivedFunctor:
  type Of[F[_]] = Functor[F] OrElse DerivedFunctor[F]

  given const[T]: DerivedFunctor[Const[T]] with
    def map[A, B](fa: T)(f: A => B): T = fa

  given composed[F[_], G[_]](using F: Of[F], G: Of[G]): DerivedFunctor[[x] =>> F[G[x]]] with
    private val underlying = F.unify `compose` G.unify
    export underlying._

  transparent inline given derived[F[_]](using K1.Instances[Of, F]): DerivedFunctor[F] =
    generic

  def generic[F[_]](using inst: K1.Instances[Of, F]): DerivedFunctor[F] =
    new Generic[Functor, F](using inst.unify) {}

  trait Generic[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends DerivedFunctor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.map(fa)(f)
      }

trait FunctorDerivation:
  extension (F: Functor.type)
    def derived[F[_]](using instance: DerivedFunctor[F]): Functor[F] = instance
