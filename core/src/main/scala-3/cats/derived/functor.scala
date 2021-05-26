package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}

object functor extends FunctorDerivation

trait DerivedFunctor[F[_]] extends Functor[F]
object DerivedFunctor:
  type Of[F[_]] = Functor[F] OrElse DerivedFunctor[F]

  given [T]: DerivedFunctor[Const[T]] with
    def map[A, B](fa: T)(f: A => B): T = fa

  def generic[F[_]](using K1.Instances[Of, F]): DerivedFunctor[F] = new Generic[Of, F] {}
  inline given derived[F[_]](using K1.Generic[F]): DerivedFunctor[F] = generic

  trait Generic[T[x[_]] <: Of[x], F[_]](using inst: K1.Instances[T, F])
    extends DerivedFunctor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.unify.map(fa)(f)
      }

trait FunctorDerivation:
  extension (F: Functor.type)
    def derived[F[_]](using instance: DerivedFunctor[F]): Functor[F] = instance
