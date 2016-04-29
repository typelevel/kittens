package cats.derived

import cats.{Foldable, Functor, Eval, Apply}, Eval.now
import alleycats.{EmptyK, Pure}
import export.{ exports, imports, reexports }
import shapeless._

// todo doesn't work because an implicit Functor needs to be scope while Apply itself implements Functor
//@reexports[MkApply]
//object apply {
//  @imports[Apply]
//  object legacy
//}

trait MkApply[F[_]] extends Apply[F]

@exports
object MkApply extends MkApply0 {
  def apply[F[_]](implicit maf: MkApply[F]): MkApply[F] = maf
}

trait MkApply0 extends MkApply1 {
  implicit def mkWithEmptyK[F[_]](
     implicit
     F: Cached[Functor[F]],
     E: Cached[EmptyK[F]],
     Fdb: Cached[Foldable[F]]
  ): MkApply[F] = doMkWithEmptyK(E.value, F.value)
}

trait MkApply1 {
  implicit def mkWithoutEmpty[F[_]](
     implicit
     F: Cached[Functor[F]],
     Fdb: Cached[Foldable[F]]
  ): MkApply[F] = {
    val nullEmpty = new EmptyK[F] {
      def empty[A]: F[A] = null.asInstanceOf[F[A]]
    }
    doMkWithEmptyK(nullEmpty, F.value)
  }

  protected def doMkWithEmptyK[F[_]](E: EmptyK[F], F: Functor[F])(
    implicit
    Fdb: Cached[Foldable[F]]
  ): MkApply[F] = new MkApply[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
      Fdb.value.foldLeft[A => B, F[B]](ff, E.empty[B])((_, f) => F.map(fa)(f))
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)

  }

}
