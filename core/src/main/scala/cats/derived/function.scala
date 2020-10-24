package cats.derived

import cats.derived.util.FnGeneric
import cats.{Distributive, Functor, Monad}
import shapeless._

object function extends FunctionInstances {

  implicit def kittensMkMonadForFunctionN[F[_], L <: _ :: _ :: _](implicit
      gen: FnGeneric.Aux[F, L]
  ): Monad[F] = new Monad[F] {
    private[this] val F = Monad[L => *]

    def pure[A](x: A): F[A] =
      gen.from(F.pure(x))

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      gen.from(F.map(gen.to(fa))(f))

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      gen.from(F.flatMap(gen.to(fa))(a => gen.to(f(a))))

    def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
      gen.from(F.tailRecM(a)(a => gen.to(f(a))))
  }
}

abstract private[derived] class FunctionInstances {

  implicit def kittensMkDistributiveForFunctionN[F[_], L <: _ :: _ :: _](implicit
      gen: FnGeneric.Aux[F, L]
  ): Distributive[F] = new Distributive[F] {
    private[this] val F = Distributive[L => *]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      gen.from(F.map(gen.to(fa))(f))

    def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]] =
      gen.from(F.distribute(ga)(a => gen.to(f(a))))
  }
}
