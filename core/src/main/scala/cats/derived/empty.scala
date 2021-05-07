package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0

object empty extends EmptyDerivation

class ProductEmpty[F[x] <: Empty[x], A](
  using inst: K0.ProductInstances[F, A]
) extends Empty[A]:
  val empty: A = inst.construct([A] => (F: F[A]) => F.empty)

trait EmptyDerivation:
  extension (E: Empty.type)
    inline def derived[A](using gen: K0.ProductGeneric[A]): Empty[A] =
      ProductEmpty(using K0.mkProductInstances)
