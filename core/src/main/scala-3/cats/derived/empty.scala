package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.compiletime.*

object empty extends EmptyDerivation

type DerivedEmpty[A] = Derived[Empty[A]]
object DerivedEmpty:
  type Or[A] = Derived.Or[Empty[A]]
  inline def apply[A]: Empty[A] =
    import DerivedEmpty.given
    summonInline[DerivedEmpty[A]].instance

  given product[A](using inst: K0.ProductInstances[Or, A]): DerivedEmpty[A] =
    Empty(inst.unify.construct([A] => (A: Empty[A]) => A.empty))

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    K0.summonFirst[Or, gen.MirroredElemTypes, A].unify

trait EmptyDerivation:
  extension (E: Empty.type)
    inline def derived[A]: Empty[A] = DerivedEmpty[A]
