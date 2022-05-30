package cats.derived

import cats.Show
import shapeless3.deriving.{Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.compiletime.*
import scala.deriving.Mirror

trait ShowPretty[A] extends Show[A]:
  def showLines(a: A): List[String]
  def show(a: A): String = showLines(a).mkString(System.lineSeparator)

object ShowPretty:
  inline def apply[A](using A: ShowPretty[A]): A.type = A

@implicitNotFound("""Could not derive an instance of ShowPretty[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
type DerivedShowPretty[A] = Derived[ShowPretty[A]]
object DerivedShowPretty:
  type Or[A] = Derived.Or[ShowPretty[A]]

  inline def apply[A]: ShowPretty[A] =
    import DerivedShowPretty.given
    summonInline[DerivedShowPretty[A]].instance

  given [A](using inst: K0.ProductInstances[Or, A], labelling: Labelling[A]): DerivedShowPretty[A] =
    given K0.ProductInstances[ShowPretty, A] = inst.unify
    new Product[ShowPretty, A] {}

  given [A](using inst: => K0.CoproductInstances[Or, A]): DerivedShowPretty[A] =
    given K0.CoproductInstances[ShowPretty, A] = inst.unify
    new Coproduct[ShowPretty, A] {}

  trait Product[F[x] <: ShowPretty[x], A](using inst: K0.ProductInstances[F, A], labelling: Labelling[A])
      extends ShowPretty[A]:
    def showLines(a: A): List[String] =
      val prefix = labelling.label
      val labels = labelling.elemLabels
      val n = labels.size
      if n <= 0 then List(s"$prefix()")
      else
        var lines: List[String] = List(")")
        val inner = inst.project(a)(n - 1)([t] => (show: F[t], x: t) => show.showLines(x))
        inner match
          case Nil => lines = s"  ${labels(n - 1)} = \"\"," :: lines
          case h :: t => lines = s"  ${labels(n - 1)} = $h" :: t.map(s => "  " + s) ::: lines
        var i = n - 2
        while i >= 0 do
          val inner = inst.project(a)(i)([t] => (show: F[t], x: t) => show.showLines(x))
          inner match
            case Nil => lines = s"  ${labels(i)} = \"\"," :: lines
            case v :: Nil => lines = s"  ${labels(i)} = $v," :: lines
            case h :: t => lines = s"  ${labels(i)} = $h" :: t.drop(1).map(s => "  " + s) ::: s"  ${t.last}," :: lines
          i -= 1

        lines = s"$prefix(" :: lines

        lines

  trait Coproduct[F[x] <: ShowPretty[x], A](using inst: K0.CoproductInstances[F, A]) extends ShowPretty[A]:
    def showLines(a: A): List[String] =
      inst.fold(a)([t] => (st: F[t], t: t) => st.showLines(t))
