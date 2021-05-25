package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0

object empty extends EmptyDerivation

trait DerivedEmpty[A] extends Empty[A]:
  protected def value(): A
  lazy val empty: A = value()

object DerivedEmpty:
  type EmptyOrDerived[A] = Empty[A] OrElse DerivedEmpty[A]

  def product[A](using inst: K0.ProductInstances[EmptyOrDerived, A]): DerivedEmpty[A] =
    () => inst.construct([A] => (F: EmptyOrDerived[A]) => F.unify.empty)

  inline def coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    () => K0.summonFirst[EmptyOrDerived, gen.MirroredElemTypes, A].unify.empty

  inline given derived[A](using gen: K0.Generic[A]): DerivedEmpty[A] =
    inline gen match
      case given K0.ProductGeneric[A] => product
      case given K0.CoproductGeneric[A] => coproduct

trait EmptyDerivation:
  extension (E: Empty.type)
    inline def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance
