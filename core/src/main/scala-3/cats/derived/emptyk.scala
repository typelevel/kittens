package cats.derived

import alleycats.{Empty, EmptyK}
import shapeless3.deriving.{Const, K1}

trait ProductEmptyK[T[x[_]] <: EmptyK[x], F[_]](
  using inst: K1.ProductInstances[T, F]
) extends EmptyK[F]:
  def empty[A]: F[A] = inst.construct([t[_]] => (emp: T[t]) => emp.empty[A])

trait EmptyKDerivation:
  extension (E: EmptyK.type)
    inline def derived[F[_]](using gen: K1.ProductGeneric[F]): EmptyK[F] =
      new ProductEmptyK[EmptyK, F]{}
