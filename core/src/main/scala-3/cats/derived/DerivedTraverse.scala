package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{Const, K1}

import scala.compiletime.*

type DerivedTraverse[F[_]] = Derived[Traverse[F]]
object DerivedTraverse:
  type Or[F[_]] = Derived.Or[Traverse[F]]
  inline def apply[F[_]]: Traverse[F] =
    import DerivedTraverse.given
    summonInline[DerivedTraverse[F]].instance

  given [T]: DerivedTraverse[Const[T]] = new Traverse[Const[T]]:
    override def map[A, B](fa: T)(f: A => B): T = fa
    override def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    override def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    override def traverse[G[_], A, B](fa: T)(f: A => G[B])(using G: Applicative[G]): G[T] = G.pure(fa)

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedTraverse[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedTraverse[F] =
    given K1.ProductInstances[Traverse, F] = inst.unify
    new Product[Traverse, F] with DerivedFunctor.Generic[Traverse, F] {}

  given [F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedTraverse[F] =
    given K1.CoproductInstances[Traverse, F] = inst.unify
    new Coproduct[Traverse, F] with DerivedFunctor.Generic[Traverse, F] {}

  trait Product[T[x[_]] <: Traverse[x], F[_]](using inst: K1.ProductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Product[T, F]:

    final override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      val pure = [a] => (x: a) => G.pure(x)
      val map = [a, b] => (ga: G[a], f: a => b) => G.map(ga)(f)
      val ap = [a, b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga)
      inst.traverse[A, G, B](fa)(map)(pure)(ap)([f[_]] => (tf: T[f], fa: f[A]) => tf.traverse(fa)(f))

  trait Coproduct[T[x[_]] <: Traverse[x], F[_]](using inst: K1.CoproductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Coproduct[T, F]:

    final override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      inst.fold(fa)([f[_]] => (tf: T[f], fa: f[A]) => tf.traverse(fa)(f).asInstanceOf[G[F[B]]])
