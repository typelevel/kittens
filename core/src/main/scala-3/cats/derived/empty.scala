package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0

trait DerivedEmpty[A] extends Empty[A]:
  protected def emptyValue(): A
  lazy val empty: A = emptyValue()

object DerivedEmpty extends DerivedEmptyLowPriority:
  given delegated[A](using A: => Empty[A]): DerivedEmpty[A] =
    () => A.empty

  def product[A](using inst: K0.ProductInstances[DerivedEmpty, A]): DerivedEmpty[A] =
    () => inst.construct([A] => (A: DerivedEmpty[A]) => A.empty)

  inline def coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    K0.summonFirst[DerivedEmpty, gen.MirroredElemTypes, A]

private[derived] sealed abstract class DerivedEmptyLowPriority:
  inline given derived[A](using gen: K0.Generic[A]): DerivedEmpty[A] =
    inline gen match
      case given K0.ProductGeneric[A] => DerivedEmpty.product
      case given K0.CoproductGeneric[A] => DerivedEmpty.coproduct

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance
