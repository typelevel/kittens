package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{Const, Continue, K1}

object traverse extends TraverseDerivation, Instances

trait DerivedTraverse[F[_]] extends Traverse[F]
object DerivedTraverse:
  type Of[F[_]] = Alt[Traverse[F], DerivedTraverse[F]]
  inline def apply[F[_]](using F: DerivedTraverse[F]): DerivedTraverse[F] = F

  given const[T]: DerivedTraverse[Const[T]] with
    override def map[A, B](fa: T)(f: A => B): T = fa
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    def traverse[G[_], A, B](fa: T)(f: A => G[B])(using G: Applicative[G]): G[T] = G.pure(fa)

  given composed[F[_], G[_]](using F: Of[F], G: Of[G]): DerivedTraverse[[x] =>> F[G[x]]] with
    private val underlying = F.unify `compose` G.unify
    export underlying.*

  given product[F[_]](using inst: K1.ProductInstances[Of, F]): DerivedTraverse[F] =
    given K1.ProductInstances[Traverse, F] = inst.unify
    new Product[Traverse, F] {}
  
  given coproduct[F[_]](using inst: => K1.CoproductInstances[Of, F]): DerivedTraverse[F] =
    given K1.CoproductInstances[Traverse, F] = inst.unify
    new Coproduct[Traverse, F] {}

  trait Product[T[x[_]] <: Traverse[x], F[_]](using inst: K1.ProductInstances[T, F])
      extends DerivedFunctor.Generic[T, F], DerivedFoldable.Product[T, F], DerivedTraverse[F]:

    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      inst.traverse[A, G, B](fa) { [a, b] => (ga: G[a], f: a => b) => 
        G.map(ga)(f)
      } { [a] => (x: a) =>
        G.pure(x)
      } { [a, b] => (gf: G[a => b], ga: G[a]) =>
        G.ap(gf)(ga)
      } { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.traverse(fa)(f)
      }

  trait Coproduct[T[x[_]] <: Traverse[x], F[_]](using inst: K1.CoproductInstances[T, F])
      extends DerivedFunctor.Generic[T, F], DerivedFoldable.Coproduct[T, F], DerivedTraverse[F]:

    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      inst.traverse[A, G, B](fa) { [a, b] => (ga: G[a], f: a => b) =>
        G.map(ga)(f)
      } { [a] => (x: a) =>
        G.pure(x)
      } { [a, b] => (gf: G[a => b], ga: G[a]) =>
        G.ap(gf)(ga)
      } { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.traverse(fa)(f)
      }

trait TraverseDerivation:
  extension (F: Traverse.type)
    def derived[F[_]](using instance: DerivedTraverse[F]): Traverse[F] = instance
