package cats.derived

import cats.Show
import shapeless3.deriving.{K0, Labelling}

import scala.annotation.*
import scala.compiletime.*

trait ShowPretty[A] extends Show[A]:
  def showLines(a: A): List[String]
  def show(a: A): String = showLines(a).mkString(System.lineSeparator)

object ShowPretty:
  inline def apply[A](using A: ShowPretty[A]): A.type = A
  def fromToString[A]: ShowPretty[A] = _.toString :: Nil

@implicitNotFound("""Could not derive an instance of ShowPretty[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
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
    import DerivedShowPretty.given
    import Strict.product
    summonInline[DerivedShowPretty[A]].instance

  // These instances support singleton types unlike the instances in Cats' core.
  given boolean[A <: Boolean]: DerivedShowPretty[A] = ShowPretty.fromToString
  given byte[A <: Byte]: DerivedShowPretty[A] = ShowPretty.fromToString
  given short[A <: Short]: DerivedShowPretty[A] = ShowPretty.fromToString
  given int[A <: Int]: DerivedShowPretty[A] = ShowPretty.fromToString
  given long[A <: Long]: DerivedShowPretty[A] = ShowPretty.fromToString
  given float[A <: Float]: DerivedShowPretty[A] = ShowPretty.fromToString
  given double[A <: Double]: DerivedShowPretty[A] = ShowPretty.fromToString
  given char[A <: Char]: DerivedShowPretty[A] = ShowPretty.fromToString
  given string[A <: String]: DerivedShowPretty[A] = ShowPretty.fromToString
  given symbol[A <: Symbol]: DerivedShowPretty[A] = ShowPretty.fromToString

  given product[A: Labelling](using K0.ProductInstances[Or, A]): DerivedShowPretty[A] =
    new Product[A] {}

  given coproduct[A](using => K0.CoproductInstances[Or, A]): DerivedShowPretty[A] =
    new Coproduct[A] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedShowPretty_A[A](using
      K0.ProductInstances[Or, A],
      Labelling[A]
  ): DerivedShowPretty[A] = product

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedShowPretty_A[A](using => K0.CoproductInstances[Or, A]): DerivedShowPretty[A] =
    coproduct

  trait Product[A](using inst: K0.ProductInstances[Or, A], labelling: Labelling[A]) extends ShowPretty[A]:
    def showLines(a: A): List[String] =
      val prefix = labelling.label
      val labels = labelling.elemLabels
      val n = labels.size
      if n <= 0 then List(s"$prefix()")
      else
        var lines = List(")")
        inst.project(a)(n - 1)([t] => (show: Or[t], x: t) => show.apply(x)) match
          case Nil => lines ::= s"  ${labels(n - 1)} = \"\","
          case h :: t => lines :::= s"  ${labels(n - 1)} = $h" :: t.map(s => "  " + s)
        var i = n - 2
        while i >= 0 do
          inst.project(a)(i)([t] => (show: Or[t], x: t) => show.apply(x)) match
            case Nil => lines ::= s"  ${labels(i)} = \"\","
            case v :: Nil => lines ::= s"  ${labels(i)} = $v,"
            case h :: t => lines = s"  ${labels(i)} = $h" :: t.init.map(s => "  " + s) ::: s"  ${t.last}," :: lines
          i -= 1
        s"$prefix(" :: lines

  trait Coproduct[A](using inst: K0.CoproductInstances[Or, A]) extends ShowPretty[A]:
    def showLines(a: A): List[String] =
      inst.fold(a)([t] => (st: Or[t], t: t) => st.apply(t))

  object Strict:
    given product[A: Labelling](using inst: K0.ProductInstances[Show, A]): DerivedShowPretty[A] =
      given K0.ProductInstances[Or, A] = inst.mapK([t] => (show: Show[t]) => Or.fromShow(show))
      DerivedShowPretty.product
