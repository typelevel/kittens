package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.compiletime.summonFrom

object empty extends EmptyDerivation

trait DerivedEmpty[A] extends Empty[A]:
  protected def emptyValue(): A
  lazy val empty: A = emptyValue()

object DerivedEmpty:
  inline given [A]: DerivedEmpty[A] = summonFrom {
    case given Empty[A] => delegated
    case given K0.ProductInstances[DerivedEmpty, A] => product
    case given K0.CoproductGeneric[A] => coproduct
  }
  
  def delegated[A](using A: => Empty[A]): DerivedEmpty[A] =
    () => A.empty

  def product[A](using inst: K0.ProductInstances[DerivedEmpty, A]): DerivedEmpty[A] =
    () => inst.construct([A] => (A: DerivedEmpty[A]) => A.empty)

  inline def coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    K0.summonFirst[DerivedEmpty, gen.MirroredElemTypes, A]

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance
