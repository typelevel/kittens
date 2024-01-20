package cats.derived

import cats.Show
import shapeless3.deriving.K0.*
import shapeless3.deriving.Labelling

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Show[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
type DerivedShow[A] = Derived[Show[A]]
object DerivedShow:
  type Or[A] = Derived.Or[Show[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Show[A] =
    import DerivedShow.given
    summonInline[DerivedShow[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Show[A] =
    import Strict.given
    summonInline[DerivedShow[A]].instance

  // These instances support singleton types unlike the instances in Cats' core.
  given boolean[A <: Boolean]: DerivedShow[A] = Show.fromToString
  given byte[A <: Byte]: DerivedShow[A] = Show.fromToString
  given short[A <: Short]: DerivedShow[A] = Show.fromToString
  given int[A <: Int]: DerivedShow[A] = Show.fromToString
  given long[A <: Long]: DerivedShow[A] = Show.fromToString
  given float[A <: Float]: DerivedShow[A] = Show.fromToString
  given double[A <: Double]: DerivedShow[A] = Show.fromToString
  given char[A <: Char]: DerivedShow[A] = Show.fromToString
  given string[A <: String]: DerivedShow[A] = Show.fromToString
  given symbol[A <: Symbol]: DerivedShow[A] = Show.fromToString

  given [A: ProductInstancesOf[DerivedShow.Or]](using labelling: Labelling[A]): DerivedShow[A] =
    Strict.product(using labelling, ProductInstances.unify)

  given [A](using => CoproductInstances[Or, A]): DerivedShow[A] =
    Strict.coproduct

  trait Product[F[x] <: Show[x], A](using inst: ProductInstances[F, A], labelling: Labelling[A]) extends Show[A]:
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

  trait Coproduct[F[x] <: Show[x], A](using inst: CoproductInstances[F, A]) extends Show[A]:
    def show(a: A): String = inst.fold(a)([t] => (st: F[t], t: t) => st.show(t))

  object Strict:
    given product[A: Labelling](using => ProductInstances[Show, A]): DerivedShow[A] =
      new Product[Show, A] {}

    given coproduct[A](using inst: => CoproductInstances[Or, A]): DerivedShow[A] =
      given CoproductInstances[Show, A] = inst.unify
      new Coproduct[Show, A] {}
