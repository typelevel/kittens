package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}

object functor extends FunctorDerivation, Instances

trait GenericFunctor[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends Functor[F]:

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    inst.map(fa : F[A])(
      [t[_]] => (func: T[t], t0: t[A]) => func.map(t0)(f)
    )

trait FunctorDerivation:
  extension (F: Functor.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Functor[F] =
      new GenericFunctor[Functor, F]{}
