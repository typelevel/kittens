package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, Const, K1}
import scala.compiletime.*

object reducible extends ReducibleDerivation

type DerivedReducible[F[_]] = Derived[Reducible[F]]
object DerivedReducible:
  type Or[F[_]] = Derived.Or[Reducible[F]]
  inline def apply[F[_]](using F: DerivedReducible[F]): Reducible[F] = F.instance

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedReducible[[x] =>> F[G[x]]] =
    F.unify `compose` G.unify

  def product[F[_]](ev: Reducible[?])(using inst: K1.ProductInstances[DerivedFoldable.Or, F]): DerivedReducible[F] =
    given K1.ProductInstances[Foldable, F] = inst.unify
    new Product[Foldable, F](ev) {}

  inline given [F[_]](using gen: K1.ProductGeneric[F]): DerivedReducible[F] =
    product(K1.summonFirst[Or, gen.MirroredElemTypes, Const[Any]].unify)

  given [F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedReducible[F] =
    given K1.CoproductInstances[Reducible, F] = inst.unify
    new Coproduct[Reducible, F] {}

  trait Product[T[x[_]] <: Foldable[x], F[_]](ev: Reducible[?])(using inst: K1.ProductInstances[T, F])
    extends DerivedFoldable.Product[T, F], Reducible[F]:

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
    extends DerivedFoldable.Coproduct[T, F], Reducible[F]:

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
    inline def derived[F[_]]: Reducible[F] =
      import DerivedFoldable.given
      import DerivedReducible.given
      summonInline[DerivedReducible[F]].instance
