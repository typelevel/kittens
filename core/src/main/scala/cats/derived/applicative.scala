package cats.derived

import alleycats.Pure
import cats.{Apply, Applicative, Foldable, Functor}

object MkApplicative {

  implicit def mkApplicative[F[_]](
    implicit
    P: MkPure[F], //on scala 2.10.6 `Pure`'s `applicativeIsPure` is somehow conflicting with auto derived Applicative and Pure
    A: Apply[F]
 ): Applicative[F] = new Applicative[F] {
    def pure[A](x: A): F[A] = P.pure(x)

    def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] = A.ap(ff)(fa)
  }

}
