package cats.derived

import cats.{Monoid, MonoidK}
import shapeless3.deriving.{Const, K1}

object monoidk extends MonoidKDerivation, Instances

trait ProductMonoidK[T[x[_]] <: MonoidK[x], F[_]](using inst: K1.ProductInstances[T, F])
  extends ProductSemigroupK[T, F], MonoidK[F]:
    def empty[A]: F[A] = inst.construct([t[_]] => (emp: T[t]) => emp.empty[A])

trait MonoidKDerivation:
  extension (F: MonoidK.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): MonoidK[F] = ???
