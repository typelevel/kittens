package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{K1, Continue}

object foldable extends FoldableDerivation

trait ProductFoldable[T[x[_]] <: Foldable[x], F[_]] extends Foldable[F]:

  val inst: K1.ProductInstances[T, F]

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    inst.foldLeft[A, B](fa)(b)(
      [t[_]] => (acc: B, fd: T[t], t0: t[A]) => Continue(fd.foldLeft(t0, acc)(f))
    )

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    //TODO update shapeless 3
    ???
    // inst.foldRight[A, Eval[B]](fa)(lb)(
    //   [t[_]] => (fd: Foldable[t], t0: t[A], acc: Eval[B]) => Continue(fd.foldRight(t0)(acc)(f))
    // )

trait CoproductFoldable[T[x[_]] <: Foldable[x], F[_]] extends Foldable[F]:

  val inst: K1.CoproductInstances[T, F]

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    inst.fold[A, B](fa)(
      [t[_]] => (fd: T[t], t0: t[A]) => fd.foldLeft(t0, b)(f)
    )

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    inst.fold[A, Eval[B]](fa)(
      [t[_]] => (fd: T[t], t0: t[A]) => fd.foldRight(t0, lb)(f)
    )

trait FoldableDerivation:
  extension (F: Foldable.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Foldable[F] =
      gen.derive(productFoldable[F], coproductFoldable[F])

  given productFoldable[F[_]](using inst: => K1.ProductInstances[Foldable, F]): Foldable[F] =
    new ProductFoldable[Foldable, F]{
        val inst: K1.ProductInstances[Foldable, F] = summon[K1.ProductInstances[Foldable, F]]
      }

  given coproductFoldable[F[_]](using inst: => K1.CoproductInstances[Foldable, F]): Foldable[F] =
    new CoproductFoldable[Foldable, F]{
        val inst: K1.CoproductInstances[Foldable, F] = summon[K1.CoproductInstances[Foldable, F]]
      }

  given Foldable[Id] with
    def foldLeft[A, B](fa: Id[A], b: B)(f: (B, A) => B): B = f(b, fa)

    def foldRight[A, B](fa: Id[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa, lb)

  given [X]: Foldable[Const[X]] with
    def foldLeft[A, B](fa: Const[X][A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[X][A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
