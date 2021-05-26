package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, Const, K1}
import cats.derived.DerivedReducible.Coproduct

object reducible extends ReducibleDerivation

trait DerivedReducible[F[_]] extends DerivedFoldable[F], Reducible[F]
object DerivedReducible:
  type Of[F[_]] = Reducible[F] OrElse DerivedReducible[F]

  def product[F[_]](ev: Of[Const[Any]])(using K1.ProductInstances[DerivedFoldable.Of, F]): DerivedReducible[F] =
    new Product[DerivedFoldable.Of, F](ev) {}

  def coproduct[F[_]](using K1.CoproductInstances[Of, F]): DerivedReducible[F] =
    new Coproduct[Of, F] {}

  inline given derived[F[_]](using gen: K1.Generic[F]): DerivedReducible[F] =
    inline gen match
      case given K1.ProductGeneric[F] => product(K1.summonFirst[Of, gen.MirroredElemTypes, Const[Any]])
      case given K1.CoproductGeneric[F] => coproduct

  trait Product[T[x[_]] <: DerivedFoldable.Of[x], F[_]](ev: Of[Const[Any]])(using inst: K1.ProductInstances[T, F])
    extends DerivedFoldable.Product[T, F], DerivedReducible[F]:

    private val none = Eval.now(None)

    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.foldLeft[A, Option[B]](fa)(None)(
        [f[_]] => (acc: Option[B], tf: T[f], fa: f[A]) =>
          acc match 
            case Some(b) => Continue(Some(tf.unify.foldLeft(fa, b)(g)))
            case None => Continue(tf.unify.reduceLeftToOption(fa)(f)(g))
      ).get

    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[Option[B]]](fa)(none)(
        [f[_]] => (tf: T[f], fa: f[A], acc: Eval[Option[B]]) =>
          Continue(acc.flatMap {
            case Some(b) => tf.unify.foldRight(fa, Eval.now(b))(g).map(Some.apply)
            case None => tf.unify.reduceRightToOption(fa)(f)(g)
          })
      ).map(_.get)

  trait Coproduct[T[x[_]] <: Of[x], F[_]](using inst: K1.CoproductInstances[T, F])
    extends DerivedFoldable.Coproduct[T, F], DerivedReducible[F]:

    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.fold[A, B](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.unify.reduceLeftTo(fa)(f)(g)
      }

    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa) { [f[_]] => (tf: T[f], fa: f[A]) =>
        Eval.defer(tf.unify.reduceRightTo(fa)(f)(g))
      }

trait ReducibleDerivation:
  extension (F: Reducible.type)
    def derived[F[_]](using instance: DerivedReducible[F]): Reducible[F] = instance
