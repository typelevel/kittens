package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{K1, Continue}

object traverse extends TraverseDerivation

trait ProductTraverse[T[x[_]] <: Traverse[x], F[_]]
    extends ProductFunctor[T, F], ProductFoldable[T, F], Traverse[F]:

  val inst: K1.ProductInstances[T, F]

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
    inst.traverse[A, G, B](fa)([a,b] => (ga: G[a], f: a => b) => G.map(ga)(f))([a] => (x: a) => G.pure(x))([a,b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga))(
      [t[_]] => (trav: T[t], t0: t[A]) => trav.traverse[G, A, B](t0)(f)
    )

trait CoproductTraverse[T[x[_]] <: Traverse[x], F[_]]
    extends ProductFunctor[T, F], CoproductFoldable[T, F], Traverse[F]:

  val inst: K1.CoproductInstances[T, F]

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
    inst.traverse[A, G, B](fa)([a,b] => (ga: G[a], f: a => b) => G.map(ga)(f))([a] => (x: a) => G.pure(x))([a,b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga))(
      [t[_]] => (trav: T[t], t0: t[A]) => trav.traverse[G, A, B](t0)(f)
    )

trait TraverseDerivation:
  extension (F: Traverse.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Traverse[F] =
      gen.derive(productTraverse, coproductTraverse)

  given productTraverse[F[_]](using inst: => K1.ProductInstances[Traverse, F]): Traverse[F] =
    new ProductTraverse[Traverse, F]{
        val inst: K1.ProductInstances[Traverse, F] = summon[K1.ProductInstances[Traverse, F]]
      }

  given coproductTraverse[F[_]](using inst: => K1.CoproductInstances[Traverse, F]): Traverse[F] =
    new CoproductTraverse[Traverse, F]{
        val inst: K1.CoproductInstances[Traverse, F] = summon[K1.CoproductInstances[Traverse, F]]
      }

  given [X]: Traverse[Const[X]] with
    override def map[A, B](fa: Const[X][A])(f: A => B): Const[X][B] = fa

    def foldLeft[A, B](fa: Const[X][A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[X][A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

    def traverse[G[_]: Applicative, A, B](fa: Const[X][A])(f: A => G[B]): G[Const[X][B]] =
      Applicative[G].pure(fa)
