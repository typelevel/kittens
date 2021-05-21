package cats.derived

import cats.Functor
import shapeless3.deriving.K1

object functor extends FunctorDerivation, Instances

trait ProductFunctor[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends Functor[F]:

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    inst.map(fa : F[A])(
      [t[_]] => (func: T[t], t0: t[A]) => func.map(t0)(f)
    )

trait FunctorDerivation:
  extension (F: Functor.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Functor[F] =
      new ProductFunctor[Functor, F]{}

  given functorGen[F[_]](using inst: => K1.ProductInstances[Functor, F]): Functor[F] =
    new ProductFunctor[Functor, F]{}
