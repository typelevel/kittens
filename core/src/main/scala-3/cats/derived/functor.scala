package cats.derived

import cats.Functor
import shapeless3.deriving.{Const, K1}
import scala.annotation.threadUnsafe

object functor extends FunctorDerivation, Instances

trait DerivedFunctor[F[_]] extends Functor[F]
object DerivedFunctor extends DerivedFunctorLowPriority:
  given const[T]: DerivedFunctor[Const[T]] with
    def map[A, B](fa: T)(f: A => B): T = fa

  given delegated[F[_]](using F: => Functor[F]): DerivedFunctor[F] =
    new Delegated(F)

  given composed[F[_]: DerivedFunctor, G[_]: DerivedFunctor]: DerivedFunctor[[x] =>> F[G[x]]] =
    new Delegated(Functor[F].compose[G])

  def generic[F[_]](using K1.Instances[DerivedFunctor, F]): DerivedFunctor[F] =
    new Generic[DerivedFunctor, F] {}

  trait Generic[T[x[_]] <: Functor[x], F[_]](using inst: K1.Instances[T, F])
    extends DerivedFunctor[F]:

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A]) { [f[_]] => (tf: T[f], fa: f[A]) =>
        tf.map(fa)(f)
      }

  private final class Delegated[F[_]](F: => Functor[F]) extends DerivedFunctor[F]:
    @threadUnsafe private lazy val underlying = F
    export underlying._

private[derived] sealed abstract class DerivedFunctorLowPriority:
  inline given derived[F[_]](using K1.Generic[F]): DerivedFunctor[F] =
    DerivedFunctor.generic

trait FunctorDerivation:
  extension (F: Functor.type)
    def derived[F[_]](using instance: DerivedFunctor[F]): Functor[F] = instance
