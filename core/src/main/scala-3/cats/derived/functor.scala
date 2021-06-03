package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}
import scala.annotation.threadUnsafe
import scala.compiletime._

object functor extends FunctorDerivation, Instances

trait DerivedFunctor[F[_]] extends Functor[F]
object DerivedFunctor:
  given const[T]: DerivedFunctor[Const[T]] with
    def map[A, B](fa: T)(f: A => B): T = fa

  // given composed[F[_]: Delegated, G[_]: Delegated]: DerivedFunctor[[x] =>> F[G[x]]] =
  //   new Delegated(Functor[F].compose[G])

  inline given derived[F[_]](using K1.Generic[F]): DerivedFunctor[F] =
    generic

  def generic[F[_]](using inst: => K1.Instances[Delegated, F]): DerivedFunctor[F] =
    new Generic[Delegated, F] {}

  trait Generic[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends DerivedFunctor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.map(fa)(f)
      }

  final class Delegated[F[_]](F: => Functor[F]) extends DerivedFunctor[F]:
    @threadUnsafe private lazy val underlying = F
    export underlying._
  object Delegated:
    inline given [F[_]]: Delegated[F] = summonFrom {
      case f: Functor[F] => Delegated(f)
      case f: DerivedFunctor[F] => Delegated(f)
    }

trait FunctorDerivation:
  extension (F: Functor.type)
    def derived[F[_]](using instance: => DerivedFunctor[F]): Functor[F] = instance
