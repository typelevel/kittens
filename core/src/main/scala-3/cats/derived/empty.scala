package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.annotation.*

object empty extends EmptyDerivation

trait DerivedEmpty[A] extends Empty[A]:
  protected def emptyValue(): A
  @threadUnsafe lazy val empty: A = emptyValue()

object DerivedEmpty:
  type Of[A] = Alt[Empty[A], DerivedEmpty[A]]

  given product[A](using inst: K0.ProductInstances[Of, A]): DerivedEmpty[A] =
    () => inst.unify.construct([A] => (A: Empty[A]) => A.empty)

  inline given coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    () => K0.summonFirst[Of, gen.MirroredElemTypes, A].unify.empty

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance
