package cats.derived

import cats.{Semigroup, SemigroupK}
import shapeless3.deriving.{Const, K1}

object semigroupk extends SemigroupKDerivation, Instances

trait ProductSemigroupK[T[x[_]] <: SemigroupK[x], F[_]](using inst: K1.ProductInstances[T, F])
  extends SemigroupK[F]:
    def combineK[A](x: F[A], y: F[A]): F[A] = inst.map2[A, A, A](x,y)(
      [t[_]] => (smgrpk: T[t], t0: t[A], t1: t[A]) => smgrpk.combineK(t0, t1)
    )

trait SemigroupKDerivation:
  extension (F: SemigroupK.type)
    inline def derived[F[_]](using gen: K1.ProductGeneric[F]): SemigroupK[F] =
      new ProductSemigroupK[SemigroupK, F]{}
