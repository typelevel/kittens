package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, Const, K1}
import scala.annotation.threadUnsafe

trait DerivedReducible[F[_]] extends Reducible[F]
object DerivedReducible extends DerivedReducibleLowPriority:
  given delegated[F[_]](using F: => Reducible[F]): DerivedReducible[F] =
    new Delegated(F)

  given composed[F[_]: DerivedReducible, G[_]: DerivedReducible]: DerivedReducible[[x] =>> F[G[x]]] =
    new Delegated(Reducible[F].compose[G])

  def product[F[_]](ev: Reducible[?])(using K1.ProductInstances[DerivedFoldable, F]): DerivedReducible[F] =
    new Product[DerivedFoldable, F](ev) {}

  def coproduct[F[_]](using K1.CoproductInstances[DerivedReducible, F]): DerivedReducible[F] =
    new Coproduct[DerivedReducible, F] {}

  trait Product[T[x[_]] <: Foldable[x], F[_]](ev: Reducible[?])(using inst: K1.ProductInstances[T, F])
    extends DerivedFoldable.Product[T, F], DerivedReducible[F]:

    private val evalNone = Eval.now(None)

    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.foldLeft[A, Option[B]](fa)(None)(
        [f[_]] => (acc: Option[B], tf: T[f], fa: f[A]) =>
          acc match 
            case Some(b) => Continue(Some(tf.foldLeft(fa, b)(g)))
            case None => Continue(tf.reduceLeftToOption(fa)(f)(g))
      ).get

    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[Option[B]]](fa)(evalNone)(
        [f[_]] => (tf: T[f], fa: f[A], acc: Eval[Option[B]]) =>
          Continue(acc.flatMap {
            case Some(b) => tf.foldRight(fa, Eval.now(b))(g).map(Some.apply)
            case None => tf.reduceRightToOption(fa)(f)(g)
          })
      ).map(_.get)

  trait Coproduct[T[x[_]] <: Reducible[x], F[_]](using inst: K1.CoproductInstances[T, F])
    extends DerivedFoldable.Coproduct[T, F], DerivedReducible[F]:

    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.fold[A, B](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.reduceLeftTo(fa)(f)(g)
      }

    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        Eval.defer(tf.reduceRightTo(fa)(f)(g))
      }

  private final class Delegated[F[_]](F: => Reducible[F]) extends DerivedReducible[F]:
    @threadUnsafe private lazy val underlying = F
    export underlying._

private[derived] sealed abstract class DerivedReducibleLowPriority:
  inline given derived[F[_]](using gen: K1.Generic[F]): DerivedReducible[F] =
    inline gen match
      case given K1.ProductGeneric[F] =>
        DerivedReducible.product(K1.summonFirst[DerivedReducible, gen.MirroredElemTypes, Const[Any]])
      case given K1.CoproductGeneric[F] => 
        DerivedReducible.coproduct

trait ReducibleDerivation:
  extension (F: Reducible.type)
    def derived[F[_]](using instance: DerivedReducible[F]): Reducible[F] = instance
