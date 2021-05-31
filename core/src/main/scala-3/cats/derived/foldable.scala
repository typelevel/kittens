package cats.derived

import cats.{Eval, Foldable}
import shapeless3.deriving.{Const, Continue, K1}
import scala.annotation.threadUnsafe

object foldable extends FoldableDerivation, Instances

trait DerivedFoldable[F[_]] extends Foldable[F]
object DerivedFoldable extends DerivedFoldableLowPriority:
  given const[T]: DerivedFoldable[Const[T]] with
    def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given delegated[F[_]](using F: => Foldable[F]): DerivedFoldable[F] =
    new Delegated(F)

  given composed[F[_]: DerivedFoldable, G[_]: DerivedFoldable]: DerivedFoldable[[x] =>> F[G[x]]] =
    new Delegated(Foldable[F].compose[G])

  def product[F[_]](using K1.ProductInstances[DerivedFoldable, F]): DerivedFoldable[F] =
    new Product[DerivedFoldable, F] {}
  
  def coproduct[F[_]](using K1.CoproductInstances[DerivedFoldable, F]): DerivedFoldable[F] = 
    new Coproduct[DerivedFoldable, F] {}

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

  private final class Delegated[F[_]](F: => Foldable[F]) extends DerivedFoldable[F]:
    @threadUnsafe private lazy val underlying = F
    export underlying._

private[derived] sealed abstract class DerivedFoldableLowPriority:
  inline given derived[F[_]](using gen: K1.Generic[F]): DerivedFoldable[F] =
    gen.derive(DerivedFoldable.product, DerivedFoldable.coproduct)

trait FoldableDerivation:
  extension (F: Foldable.type)
    def derived[F[_]](using instance: DerivedFoldable[F]): Foldable[F] = instance
