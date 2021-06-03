package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, Const, K1}

object reducible extends ReducibleDerivation

trait DerivedReducible[F[_]] extends Reducible[F]
object DerivedReducible:
  type Of[F[_]] = Alt[Reducible[F], DerivedReducible[F]]
  inline def apply[F[_]](using F: DerivedReducible[F]): DerivedReducible[F] = F

  given composed[F[_], G[_]](using F: Of[F], G: Of[G]): DerivedReducible[[x] =>> F[G[x]]] with
    private val underlying = F.unify `compose` G.unify
    export underlying.*

  def product[F[_]](ev: Reducible[?])(using inst: K1.ProductInstances[DerivedFoldable.Of, F]): DerivedReducible[F] =
    given K1.ProductInstances[Foldable, F] = inst.unify
    new Product[Foldable, F](ev) {}

  given coproduct[F[_]](using inst: => K1.CoproductInstances[Of, F]): DerivedReducible[F] =
    given K1.CoproductInstances[Reducible, F] = inst.unify
    new Coproduct[Reducible, F] {}

  inline given [F[_]](using gen: K1.ProductGeneric[F]): DerivedReducible[F] =
    product(K1.summonFirst[Of, gen.MirroredElemTypes, Const[Any]].unify)

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

trait ReducibleDerivation:
  extension (F: Reducible.type)
    def derived[F[_]](using instance: DerivedReducible[F]): Reducible[F] = instance
