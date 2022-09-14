package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{Const, Continue, K1}

import scala.annotation.implicitNotFound
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Foldable[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T
  * it is a nested type [x] =>> G[H[x]] where G: Foldable and H: Foldable
  * it is a generic case class where all fields have a Foldable instance
  * it is a generic sealed trait where all subclasses have a Foldable instance""")
type DerivedFoldable[F[_]] = Derived[Foldable[F]]
object DerivedFoldable:
  type Or[F[_]] = Derived.Or[Foldable[F]]
  inline def apply[F[_]]: Foldable[F] =
    import DerivedFoldable.given
    summonInline[DerivedFoldable[F]].instance

  given [T]: DerivedFoldable[Const[T]] = new Foldable[Const[T]]:
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedFoldable[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedFoldable[F] =
    given K1.ProductInstances[Foldable, F] = inst.unify
    new Product[Foldable, F] {}

  given [F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedFoldable[F] =
    given K1.CoproductInstances[Foldable, F] = inst.unify
    new Coproduct[Foldable, F] {}

  trait Product[T[x[_]] <: Foldable[x], F[_]](using inst: K1.ProductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft[A, B](fa)(b)([f[_]] => (acc: B, tf: T[f], fa: f[A]) => Continue(tf.foldLeft(fa, acc)(f)))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[B]](fa)(lb)(
        [f[_]] => (tf: T[f], fa: f[A], acc: Eval[B]) => Continue(Eval.defer(tf.foldRight(fa, acc)(f)))
      )

  trait Coproduct[T[x[_]] <: Foldable[x], F[_]](using inst: K1.CoproductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold[A, B](fa)([f[_]] => (tf: T[f], fa: f[A]) => tf.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa)([f[_]] => (tf: T[f], fa: f[A]) => Eval.defer(tf.foldRight(fa, lb)(f)))
