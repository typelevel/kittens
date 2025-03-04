package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Traverse for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Traverse and H: Traverse
  * generic case class where all fields form Traverse
  * generic sealed trait where all subclasses form Traverse
  * generic enum where all variants form Traverse""")
type DerivedTraverse[F[_]] = Derived[Traverse[F]]
object DerivedTraverse:
  @nowarn("msg=unused import")
  inline def apply[F[_]]: Traverse[F] =
    import DerivedTraverse.given
    summonInline[DerivedTraverse[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Traverse[F] =
    import Strict.given
    summonInline[DerivedTraverse[F]].instance

  given [T]: DerivedTraverse[Const[T]] = new Traverse[Const[T]]:
    override def map[A, B](fa: T)(f: A => B): T = fa
    override def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    override def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    override def traverse[G[_], A, B](fa: T)(f: A => G[B])(using G: Applicative[G]): G[T] = G.pure(fa)

  given nested[F[_], G[_]](using
      F: => (Traverse |: Derived)[F],
      G: => (Traverse |: Derived)[G]
  ): DerivedTraverse[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Traverse[F <<< G]:
      export delegate.*

  given [F[_]](using inst: ProductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
    Strict.product(using inst.unify)

  given [F[_]](using => CoproductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Traverse |: Derived, G[_]: Traverse |: Derived]: DerivedTraverse[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Traverse[f], F[_]](using inst: ProductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Product[T, F]:

    final override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      val pure = [a] => (x: a) => G.pure(x)
      val map = [a, b] => (ga: G[a], f: a => b) => G.map(ga)(f)
      val ap = [a, b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga)
      inst.traverse[A, G, B](fa)(map)(pure)(ap)([f[_]] => (F: T[f], fa: f[A]) => F.traverse(fa)(f))

  trait Coproduct[T[f[_]] <: Traverse[f], F[_]](using inst: CoproductInstances[T, F])
      extends Traverse[F],
        DerivedFunctor.Generic[T, F],
        DerivedFoldable.Coproduct[T, F]:

    final override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      inst.fold(fa)([f[a] <: F[a]] => (F: T[f], fa: f[A]) => G.widen[f[B], F[B]](F.traverse(fa)(f)))

  object Strict:
    given product[F[_]: ProductInstancesOf[Traverse]]: DerivedTraverse[F] =
      new Traverse[F] with Product[Traverse, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Traverse |: Derived, F]): DerivedTraverse[F] =
      given CoproductInstances[Traverse, F] = inst.unify
      new Traverse[F] with Coproduct[Traverse, F] {}
