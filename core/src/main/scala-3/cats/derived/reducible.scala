package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, K1}

object reducible extends ReducibleDerivation

trait ProductReducible[T[x[_]] <: Foldable[x], F[_]](ev: Any)(using inst: K1.ProductInstances[T, F])
    extends ProductFoldable[T, F], Reducible[F]:

  private val none = Eval.now(None)

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
    inst.foldLeft[A, Option[B]](fa)(None)(
      [t[_]] => (acc: Option[B], fd: T[t], t0: t[A]) =>
        acc match 
          case Some(b) => Continue(Some(fd.foldLeft(t0, b)(g)))
          case None => Continue(fd.reduceLeftToOption(t0)(f)(g))
    ).get

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    inst.foldRight[A, Eval[Option[B]]](fa)(none)(
      [t[_]] => (fd: T[t], t0: t[A], acc: Eval[Option[B]]) =>
        Continue(acc.flatMap {
          case Some(b) => fd.foldRight(t0, Eval.now(b))(g).map(Some.apply)
          case None => fd.reduceRightToOption(t0)(f)(g)
        })
    ).map(_.get)

object ProductReducible:
  def instance[F[_]](ev: Any)(using gen: K1.ProductInstances[Foldable, F]): ProductReducible[Foldable, F] =
    new ProductReducible[Foldable, F](ev){}

trait CoproductReducible[T[x[_]] <: Reducible[x], F[_]](using inst: K1.CoproductInstances[T, F])
    extends CoproductFoldable[T, F], Reducible[F]:

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
    inst.fold[A, B](fa)(
      [t[_]] => (fd: T[t], t0: t[A]) => fd.reduceLeftTo(t0)(f)(g)
    )

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    inst.fold[A, Eval[B]](fa)(
      [t[_]] => (fd: T[t], t0: t[A]) => fd.reduceRightTo(t0)(f)(g)
    )

object CoproductReducible:
  given instance[F[_]](using K1.CoproductInstances[Reducible, F]): CoproductReducible[Reducible, F] with {}

trait ReducibleDerivation:
  extension (F: Reducible.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Reducible[F] =
      inline gen match
        case given K1.ProductGeneric[F] =>
          val ev = K1.summonFirst0[K1.LiftP[Reducible, gen.MirroredElemTypes]]
          ProductReducible.instance(ev)
        case given K1.CoproductGeneric[F] => CoproductReducible.instance
    
