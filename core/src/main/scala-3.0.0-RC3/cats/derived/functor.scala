package cats.derived

import cats.Functor
import shapeless3.deriving.K1

object functor extends FunctorDerivation

class ProductFunctor[T[x[_]] <: Functor[x], F[_]](
  using inst: => K1.Instances[T, F]
) extends Functor[F]:
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    inst.map(fa : F[A])(
      [t[_]] => (func: T[t], t0: t[A]) => func.map(t0)(f)
    )

trait FunctorDerivation:
  extension (F: Functor.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Functor[F] =
      ProductFunctor[Functor, F]

  given functorGen[F[_]](using inst: => K1.ProductInstances[Functor, F]): Functor[F] =
    ProductFunctor[Functor, F]

  given Functor[Id] with
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

  given [X]: Functor[Const[X]] with
      def map[A, B](fa: Const[X][A])(f: A => B): Const[X][B] = fa
