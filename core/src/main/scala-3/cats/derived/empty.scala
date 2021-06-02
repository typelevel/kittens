package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.compiletime.*

object empty extends EmptyDerivation

trait DerivedEmpty[A] extends Empty[A]:
  protected def emptyValue(): A
  lazy val empty: A = emptyValue()

object DerivedEmpty:
  type Of[A] = Empty[A] OrElse DerivedEmpty[A]

  inline given derived[A]: DerivedEmpty[A] = summonFrom {
    case given K0.ProductInstances[Of, A] => product
    case given K0.CoproductGeneric[A] => coproduct
  }

  def product[A](using inst: K0.ProductInstances[Of, A]): DerivedEmpty[A] =
    () => inst.construct([A] => (A: Of[A]) => A.unify.empty)

  inline def coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    () => K0.summonFirst[Of, gen.MirroredElemTypes, A].unify.empty

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = instance
