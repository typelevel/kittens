package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{Const, Continue, K1}
import scala.annotation.threadUnsafe

trait DerivedTraverse[F[_]] extends Traverse[F]
object DerivedTraverse extends DerivedTraverseLowPriority:
  given const[T]: DerivedTraverse[Const[T]] with
    override def map[A, B](fa: T)(f: A => B): T = fa
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    def traverse[G[_], A, B](fa: T)(f: A => G[B])(using G: Applicative[G]): G[T] = G.pure(fa)

  given delegated[F[_]](using F: => Traverse[F]): DerivedTraverse[F] =
    new Delegated(F)

  given composed[F[_]: DerivedTraverse, G[_]: DerivedTraverse]: DerivedTraverse[[x] =>> F[G[x]]] =
    new Delegated(Traverse[F].compose[G])

  def product[F[_]](using K1.ProductInstances[DerivedTraverse, F]): DerivedTraverse[F] =
    new Product[DerivedTraverse, F] {}
  
  def coproduct[F[_]](using K1.CoproductInstances[DerivedTraverse, F]): DerivedTraverse[F] =
    new Coproduct[DerivedTraverse, F] {}

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

  private final class Delegated[F[_]](F: => Traverse[F]) extends DerivedTraverse[F]:
    @threadUnsafe private lazy val underlying = F
    export underlying._

private[derived] sealed abstract class DerivedTraverseLowPriority:
  inline given derived[F[_]](using gen: K1.Generic[F]): DerivedTraverse[F] =
    gen.derive(DerivedTraverse.product, DerivedTraverse.coproduct)

trait TraverseDerivation:
  extension (F: Traverse.type)
    def derived[F[_]](using instance: DerivedTraverse[F]): Traverse[F] = instance
