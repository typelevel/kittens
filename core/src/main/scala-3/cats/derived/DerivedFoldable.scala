package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.K1.*
import shapeless3.deriving.Const

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Foldable for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Foldable and H: Foldable
  * generic case class where all fields form Foldable
  * generic sealed trait where all subclasses form Foldable
  * generic enum where all variants form Foldable""")
type DerivedFoldable[F[_]] = Derived[Foldable[F]]
object DerivedFoldable:
  @nowarn("msg=unused import")
  inline def apply[F[_]]: Foldable[F] =
    import DerivedFoldable.given
    summonInline[DerivedFoldable[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Foldable[F] =
    import Strict.given
    summonInline[DerivedFoldable[F]].instance

  given [T]: DerivedFoldable[Const[T]] = new Foldable[Const[T]]:
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given nested[F[_], G[_]](using
      F: => Derived.Or[Foldable[F]],
      G: => Derived.Or[Foldable[G]]
  ): DerivedFoldable[F <<< G] =
    new Derived.Lazy(() => F.compose(using G)) with Foldable[F <<< G]:
      export delegate.*

  given [F[_]: ProductInstancesOf[Derived.Or1[Foldable]]]: DerivedFoldable[F] =
    Strict.product

  given [F[_]](using => CoproductInstances[Derived.Or1[Foldable], F]): DerivedFoldable[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Derived.Or1[Foldable], G[_]: Derived.Or1[Foldable]]: DerivedFoldable[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Foldable[f], F[_]](using inst: ProductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.foldLeft(fa)(b)([f[_]] => (b: B, F: T[f], fa: f[A]) => F.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight(fa)(lb)([f[_]] => (F: T[f], fa: f[A], lb: Eval[B]) => Eval.defer(F.foldRight(fa, lb)(f)))

  trait Coproduct[T[f[_]] <: Foldable[f], F[_]](using inst: CoproductInstances[T, F]) extends Foldable[F]:
    final override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.foldLeft(fa, b)(f))

    final override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.foldRight(fa, lb)(f)))

  object Strict:
    given product[F[_]: ProductInstancesOf[Foldable]]: DerivedFoldable[F] =
      new Product[Foldable, F] {}

    given coproduct[F[_]](using inst: => CoproductInstances[Derived.Or1[Foldable], F]): DerivedFoldable[F] =
      new Coproduct[Foldable, F] {}
