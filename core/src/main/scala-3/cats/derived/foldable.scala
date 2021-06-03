package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{Const, Continue, K1}

object foldable extends FoldableDerivation, Instances

trait DerivedFoldable[F[_]] extends Foldable[F]
object DerivedFoldable:
  type Of[F[_]] = Alt[Foldable[F], DerivedFoldable[F]]
  inline def apply[F[_]](using F: DerivedFoldable[F]): DerivedFoldable[F] = F

  given const[T]: DerivedFoldable[Const[T]] with
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given composed[F[_], G[_]](using F: Of[F], G: Of[G]): DerivedFoldable[[x] =>> F[G[x]]] with
    private val underlying = F.unify `compose` G.unify
    export underlying.*

  given product[F[_]](using inst: K1.ProductInstances[Of, F]): DerivedFoldable[F] =
    new Product(using inst.unify) {}
  
  given coproduct[F[_]](using inst: => K1.CoproductInstances[Of, F]): DerivedFoldable[F] = 
    new Coproduct(using inst.unify) {}

  trait Product[T[x[_]] <: Foldable[x], F[_]](using inst: K1.ProductInstances[T, F])
    extends DerivedFoldable[F]:

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft[A, B](fa)(b) { [f[_]] => (acc: B, tf: T[f], fa: f[A]) => 
        Continue(tf.foldLeft(fa, acc)(f))
      }

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[B]](fa)(lb) { [f[_]] => (tf: T[f], fa: f[A], acc: Eval[B]) =>
        Continue(Eval.defer(tf.foldRight(fa, acc)(f)))
      }

  trait Coproduct[T[x[_]] <: Foldable[x], F[_]](using inst: K1.CoproductInstances[T, F])
    extends DerivedFoldable[F]:

    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold[A, B](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.foldLeft(fa, b)(f)
      }

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        Eval.defer(tf.foldRight(fa, lb)(f))
      }

trait FoldableDerivation:
  extension (F: Foldable.type)
    def derived[F[_]](using instance: DerivedFoldable[F]): Foldable[F] = instance
