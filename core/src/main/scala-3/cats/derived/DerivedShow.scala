package cats.derived

import cats.{Eval, Show}
import shapeless3.deriving.{Derived, Labelling}
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Show for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Show
  * sealed trait where all subclasses form Show
  * enum where all variants form Show""")
type DerivedShow[A] = Derived[Show[A]]
object DerivedShow:
  inline def apply[A]: Show[A] =
    import DerivedShow.given
    summonInline[DerivedShow[A]].instance

  inline def strict[A]: Show[A] =
    import Strict.given
    summonInline[DerivedShow[A]].instance

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

  given [A](using inst: ProductInstances[Show |: Derived, A], labelling: Labelling[A]): DerivedShow[A] =
    given ProductInstances[Show, A] = inst.unify
    Strict.product

  given [A](using => CoproductInstances[Show |: Derived, A]): DerivedShow[A] =
    Strict.coproduct

  private[derived] trait Safe[A] extends Show[A]:
    private[derived] def safeShow(a: A): Eval[String]
    override def show(a: A): String = safeShow(a).value

  private[derived] def safeShow[A](F: Show[A])(a: A): Eval[String] =
    F match
      case safe: Safe[?] => safe.asInstanceOf[Safe[A]].safeShow(a)
      case _ => Eval.later(F.show(a))

  trait Product[F[x] <: Show[x], A](using inst: ProductInstances[F, A], labelling: Labelling[A]) extends Safe[A]:
    private[derived] final override def safeShow(a: A): Eval[String] =
      val prefix = labelling.label
      val labels = labelling.elemLabels
      val n = labels.size
      if n <= 0 then Eval.now(prefix)
      else
        val sb = new StringBuilder(prefix)
        sb.append('(')
        def loop(i: Int): Eval[StringBuilder] =
          if i >= n then Eval.now(sb)
          else
            inst.project(a)(i)([t] => (showt: F[t], xt: t) => DerivedShow.safeShow(showt)(xt)).flatMap: rendered =>
              sb.append(labels(i))
              sb.append(" = ")
              sb.append(rendered)
              if i < n - 1 then sb.append(", ")
              Eval.defer(loop(i + 1))
        loop(0).map: built =>
          built.append(')')
          built.toString

  trait Coproduct[F[x] <: Show[x], A](using inst: CoproductInstances[F, A]) extends Safe[A]:
    private[derived] final override def safeShow(a: A): Eval[String] =
      Eval.defer(inst.fold[Eval[String]](a)([t] => (st: F[t], t: t) => DerivedShow.safeShow(st)(t)))

  object Strict:
    given product[A: Labelling](using => ProductInstances[Show, A]): DerivedShow[A] =
      new Product[Show, A] {}

    given coproduct[A](using inst: => CoproductInstances[Show |: Derived, A]): DerivedShow[A] =
      given CoproductInstances[Show, A] = inst.unify
      new Coproduct[Show, A] {}
