package cats.derived

import alleycats.Empty
import cats.derived.util.Kinds
import shapeless3.deriving.K0

import scala.compiletime.*

type DerivedEmpty[A] = Derived[Empty[A]]
object DerivedEmpty:
  type Or[A] = Derived.Or[Empty[A]]
  inline def apply[A]: Empty[A] =
    import DerivedEmpty.given
    summonInline[DerivedEmpty[A]].instance

  given product[A](using inst: K0.ProductInstances[Or, A]): DerivedEmpty[A] =
    Empty(inst.unify.construct([A] => (A: Empty[A]) => A.empty))

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    Kinds.summonOne0[Or, gen.MirroredElemTypes, A].unify
