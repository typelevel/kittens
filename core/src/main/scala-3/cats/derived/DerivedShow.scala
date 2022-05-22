package cats.derived

import cats.Show
import shapeless3.deriving.{Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.compiletime.*
import scala.deriving.Mirror

@implicitNotFound("""Could not derive an instance of Show[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
type DerivedShow[A] = Derived[Show[A]]
object DerivedShow:
  type Or[A] = Derived.Or[Show[A]]

  inline def apply[A]: Show[A] =
    import DerivedShow.given
    summonInline[DerivedShow[A]].instance

  given [A](using inst: K0.ProductInstances[Or, A], labelling: Labelling[A]): DerivedShow[A] =
    given K0.ProductInstances[Show, A] = inst.unify
    new Product[Show, A] {}

  given [A](using inst: => K0.CoproductInstances[Or, A]): DerivedShow[A] =
    given K0.CoproductInstances[Show, A] = inst.unify
    new Coproduct[Show, A] {}

  trait Product[F[x] <: Show[x], A](using inst: K0.ProductInstances[F, A], labelling: Labelling[A]) extends Show[A]:
    def show(a: A): String =
      val prefix = labelling.label
      val labels = labelling.elemLabels
      val n = labels.size
      if n <= 0 then prefix
      else
        val sb = new StringBuilder(prefix)
        sb.append('(')
        var i = 0
        while i < n do
          sb.append(labels(i))
          sb.append(" = ")
          sb.append(inst.project(a)(i)([t] => (show: F[t], x: t) => show.show(x)))
          sb.append(", ")
          i += 1

        val l = sb.length
        sb.delete(l - 2, l)
        sb.append(')')
        sb.toString

  trait Coproduct[F[x] <: Show[x], A](using inst: K0.CoproductInstances[F, A]) extends Show[A]:
    def show(a: A): String =
      inst.fold(a)([t] => (st: F[t], t: t) => st.show(t))
