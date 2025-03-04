package cats.derived

import cats.Show
import shapeless3.deriving.{Derived, Labelling}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

trait ShowPretty[A] extends Show[A]:
  def showLines(a: A): List[String]
  def show(a: A): String = showLines(a).mkString(System.lineSeparator)

object ShowPretty:
  inline def apply[A](using A: ShowPretty[A]): A.type = A

@implicitNotFound("""Could not derive ShowPretty for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Show
  * sealed trait where all subclasses form Show
  * enum where all variants form Show""")
type DerivedShowPretty[A] = Derived[ShowPretty[A]]
object DerivedShowPretty:
  opaque type Or[A] = A => List[String]
  object Or:
    extension [A](or: Or[A]) def apply(a: A): List[String] = or(a)
    inline given [A]: Or[A] = summonFrom:
      case instance: Show[A] => fromShow(instance)
      case derived: DerivedShowPretty[A] => fromShow(derived.instance)
    private[derived] def fromShow[A](instance: Show[A]): Or[A] = instance match
      case pretty: ShowPretty[A] => pretty.showLines
      case _ => instance.show(_).split(System.lineSeparator).toList

  @nowarn("msg=unused import")
  inline def apply[A]: ShowPretty[A] =
    import DerivedShowPretty.given
    summonInline[DerivedShowPretty[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: ShowPretty[A] =
    import Strict.given
    summonInline[DerivedShowPretty[A]].instance

  private def fromToString[A]: ShowPretty[A] =
    _.toString :: Nil

  // These instances support singleton types unlike the instances in Cats' core.
  given boolean[A <: Boolean]: DerivedShowPretty[A] = fromToString
  given byte[A <: Byte]: DerivedShowPretty[A] = fromToString
  given short[A <: Short]: DerivedShowPretty[A] = fromToString
  given int[A <: Int]: DerivedShowPretty[A] = fromToString
  given long[A <: Long]: DerivedShowPretty[A] = fromToString
  given float[A <: Float]: DerivedShowPretty[A] = fromToString
  given double[A <: Double]: DerivedShowPretty[A] = fromToString
  given char[A <: Char]: DerivedShowPretty[A] = fromToString
  given string[A <: String]: DerivedShowPretty[A] = fromToString
  given symbol[A <: Symbol]: DerivedShowPretty[A] = fromToString

  given product[A: Labelling](using => ProductInstances[Or, A]): DerivedShowPretty[A] = new Product[A] {}
  given coproduct[A](using => CoproductInstances[Or, A]): DerivedShowPretty[A] = new Coproduct[A] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [A: ProductInstancesOf[DerivedShowPretty.Or]: Labelling]: DerivedShowPretty[A] = product

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [A](using => CoproductInstances[Or, A]): DerivedShowPretty[A] = coproduct

  trait Product[A](using inst: ProductInstances[Or, A], labelling: Labelling[A]) extends ShowPretty[A]:
    def showLines(a: A): List[String] =
      val prefix = labelling.label
      val labels = labelling.elemLabels
      val n = labels.size
      if n <= 0 then List(s"$prefix()")
      else
        var lines = List(")")
        inst.project(a)(n - 1)([a] => (show: Or[a], x: a) => show(x)) match
          case Nil => lines ::= s"  ${labels(n - 1)} = \"\","
          case h :: t => lines :::= s"  ${labels(n - 1)} = $h" :: t.map(s => "  " + s)
        var i = n - 2
        while i >= 0 do
          inst.project(a)(i)([a] => (show: Or[a], x: a) => show(x)) match
            case Nil => lines ::= s"  ${labels(i)} = \"\","
            case v :: Nil => lines ::= s"  ${labels(i)} = $v,"
            case h :: t => lines = s"  ${labels(i)} = $h" :: t.init.map(s => "  " + s) ::: s"  ${t.last}," :: lines
          i -= 1
        s"$prefix(" :: lines

  trait Coproduct[A](using inst: CoproductInstances[Or, A]) extends ShowPretty[A]:
    def showLines(a: A): List[String] = inst.fold(a)([a] => (show: DerivedShowPretty.Or[a], x: a) => show(x))

  object Strict:
    export DerivedShowPretty.coproduct
    given product[A: Labelling](using inst: => ProductInstances[Show, A]): DerivedShowPretty[A] =
      given ProductInstances[Or, A] = inst.mapK([a] => (show: Show[a]) => DerivedShowPretty.Or.fromShow(show))
      DerivedShowPretty.product
