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

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedFoldable[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose(G.unify)) with Foldable[[x] =>> F[G[x]]]:
      export delegate.*

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedFoldable[F] =
    given K1.ProductInstances[Foldable, F] = inst.unify
    new Product[Foldable, F] {}

  given [F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedFoldable[F] =
    given K1.CoproductInstances[Foldable, F] = inst.unify
    new Coproduct[Foldable, F] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedFoldable_F[F[_]: Or, G[_]: Or]: DerivedFoldable[[x] =>> F[G[x]]] = summon

  trait Product[T[f[_]] <: Foldable[f], F[_]](using inst: K1.ProductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft(fa)(b)([f[_]] => (b: B, F: T[f], fa: f[A]) => Continue(F.foldLeft(fa, b)(f)))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight(fa)(lb):
        [f[_]] => (F: T[f], fa: f[A], lb: Eval[B]) => Continue(Eval.defer(F.foldRight(fa, lb)(f)))

  trait Coproduct[T[f[_]] <: Foldable[f], F[_]](using inst: K1.CoproductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.foldRight(fa, lb)(f)))
