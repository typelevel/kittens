package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{Const, Continue, K1}

object foldable extends FoldableDerivation, Instances

trait DerivedFoldable[F[_]] extends Foldable[F]
object DerivedFoldable:
  type Of[F[_]] = Foldable[F] OrElse DerivedFoldable[F]

  given [T]: DerivedFoldable[Const[T]] with
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  def product[F[_]](using K1.ProductInstances[Of, F]): DerivedFoldable[F] = new Product[Of, F] {}
  def coproduct[F[_]](using K1.CoproductInstances[Of, F]): DerivedFoldable[F] = new Coproduct[Of, F] {}

  inline given derived[F[_]](using gen: K1.Generic[F]): DerivedFoldable[F] =
    gen.derive(product, coproduct)

  trait Product[T[x[_]] <: Of[x], F[_]](using inst: K1.ProductInstances[T, F])
    extends DerivedFoldable[F]:

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft[A, B](fa)(b) { [f[_]] => (acc: B, tf: T[f], fa: f[A]) => 
        Continue(tf.unify.foldLeft(fa, acc)(f))
      }

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[B]](fa)(lb) { [f[_]] => (tf: T[f], fa: f[A], acc: Eval[B]) =>
        Continue(Eval.defer(tf.unify.foldRight(fa, acc)(f)))
      }

  trait Coproduct[T[x[_]] <: Of[x], F[_]](using inst: K1.CoproductInstances[T, F])
    extends DerivedFoldable[F]:

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold[A, B](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.unify.foldLeft(fa, b)(f)
      }

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        Eval.defer(tf.unify.foldRight(fa, lb)(f))
      }

trait FoldableDerivation:
  extension (F: Foldable.type)
    def derived[F[_]](using instance: DerivedFoldable[F]): Foldable[F] = instance
