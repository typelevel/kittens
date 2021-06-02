package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}
import scala.compiletime.*

object functor extends FunctorDerivation, Instances

trait DerivedFunctor[F[_]] extends Functor[F]
object DerivedFunctor:
  type Of[F[_]] = Alt1[Functor, DerivedFunctor, F]

  given const[T]: DerivedFunctor[Const[T]] with
    def map[A, B](fa: T)(f: A => B): T = fa

  given composed[F[_], G[_]](using F: Of[F], G: Of[G]): DerivedFunctor[[x] =>> F[G[x]]] with
    private val underlying = F.unify `compose` G.unify
    export underlying._

  inline given derived[F[_]]: DerivedFunctor[F] = summonFrom {
    case given K1.ProductInstances[Of, F] => generic
    case given K1.CoproductInstances[Of, F] => generic
  }

  def generic[F[_]](using K1.Instances[Of, F]): DerivedFunctor[F] =
    new Generic[Of, F] {}

  trait Generic[T[x[_]] <: Of[x], F[_]](using inst: K1.Instances[T, F])
    extends DerivedFunctor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.unify.map(fa)(f)
      }

trait FunctorDerivation:
  extension (F: Functor.type)
    def derived[F[_]](using instance: DerivedFunctor[F]): Functor[F] = instance
