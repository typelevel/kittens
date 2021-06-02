package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0
import scala.compiletime.summonFrom
import scala.util.NotGiven

object empty extends EmptyDerivation

trait DerivedEmpty[A] extends Empty[A]:
  protected def emptyValue(): A
  lazy val empty: A = emptyValue()

enum EmptyOrDerived[A]:
  case L(empty: Empty[A])
  case R(derived: DerivedEmpty[A])

object EmptyOrDerived:
  inline given [A]: EmptyOrDerived[A] = summonFrom {
    case e: Empty[A] => L(e)
    case d: DerivedEmpty[A] => R(d)
  }

object DerivedEmpty:
  inline given [A]: DerivedEmpty[A] = summonFrom {
    case given K0.ProductInstances[EmptyOrDerived, A] => product
    case given K0.CoproductGeneric[A] => coproduct
  }

  def product[A](using inst: K0.ProductInstances[EmptyOrDerived, A]): DerivedEmpty[A] =
    () => inst.construct([A] => (A: EmptyOrDerived[A]) =>
    A match {
      case EmptyOrDerived.L(e) => e.empty
      case EmptyOrDerived.R(d) => d.empty
    })

  inline def coproduct[A](using gen: K0.CoproductGeneric[A]): DerivedEmpty[A] =
    K0.summonFirst[EmptyOrDerived, gen.MirroredElemTypes, A] match {
      case EmptyOrDerived.L(e) => () => e.empty
      case EmptyOrDerived.R(d) => d
    }

trait EmptyDerivation:
  extension (E: Empty.type)
    def derived[A](using instance: DerivedEmpty[A]): Empty[A] = 
      Empty(instance.empty)


import empty.derived

case class Foo(i: Int, b: IntTree) derives Empty
enum IntTree:
  case Leaf
  case Node(left: IntTree, value: Int, right: IntTree)

@main def test = 
  println(Empty[Foo].empty)
