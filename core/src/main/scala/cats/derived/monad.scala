package cats.derived

import cats._, Eval.now
import alleycats.{ConsK, EmptyK, Pure}
import export.{ exports, imports, reexports }
import shapeless._


@reexports[MkMonad]
object monad {
  @imports[Monad]
  object legacy
}

trait MkMonad[F[_]] extends Monad[F]

@exports
object MkMonad extends MkMonad0 {
  def apply[F[_]](implicit mmf: MkMonad[F]): MkMonad[F] = mmf
}

trait MkMonad0 extends MkMonad1 {
  implicit def withConsK[F[_]](
    implicit
    P: Cached[Pure[F]],
    C: Cached[ConsK[F]],
    E: Cached[EmptyK[F]],
    F: Cached[Foldable[F]]
  ): MkMonad[F] = new MkMonad[F] {
    def pure[A](x: A): F[A] = P.value.pure(x)

    def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
      F.value.foldRight[A, F[B]](fa, now(E.value.empty[B])) { (a, l) =>
        val fb = f(a)
        F.value.foldRight(fb, l)((b, memo) => now(C.value.cons(b, memo.value)))
      }.value
    }
  }
}


trait MkMonad1 {
  implicit def withoutConsK[F[_]](
    implicit
    P: Cached[Pure[F]],
    E: Cached[EmptyK[F]],
    F: Cached[Foldable[F]]
  ): MkMonad[F] = new MkMonad[F] {
    def pure[A](x: A): F[A] = P.value.pure(x)

    def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
      F.value.foldLeft[A, F[B]](fa, E.value.empty[B])((_, a) => f(a))
    }
  }
}
