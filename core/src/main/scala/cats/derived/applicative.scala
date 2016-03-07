package cats.derived

import cats.{Foldable, Functor, Eval, Applicative}, Eval.now
import alleycats.Pure
import export.{ exports, imports, reexports }
import shapeless._


@reexports[MkApplicative]
object applicative {
  @imports[Applicative]
  object legacy
}

trait MkApplicative[F[_]] extends Applicative[F]

@exports
object MkApplicative {
  def apply[F[_]](implicit maf: MkApplicative[F]): MkApplicative[F] = maf

  implicit def fromFunctorPureAndFoldable[F[_]](
     implicit
     F: Functor[F],
     P: Pure[F],
     Fdb: Foldable[F]
  ): MkApplicative[F] = new MkApplicative[F] {
    def pure[A](x: A): F[A] = P.pure(x)

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
      val ph : F[B] = null.asInstanceOf[F[B]]
      Fdb.foldLeft[A => B, F[B]](ff, ph)((_, f) => F.map(fa)(f))
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val ph: F[(A, B)] = null.asInstanceOf[F[(A,B)]]
      Fdb.foldLeft[A, F[(A, B)]](fa, ph)((_, a) => F.map(fb)((a, _)))
    }
  }
}
