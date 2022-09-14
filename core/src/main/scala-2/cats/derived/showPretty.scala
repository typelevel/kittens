package cats.derived

import cats.Show
import cats.derived.util.VersionSpecific.{Lazy, OrElse}
import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

trait ShowPretty[A] extends Show[A] {
  def showLines(a: A): List[String]
  def show(a: A): String = showLines(a).mkString(System.lineSeparator)
}

object ShowPretty {
  def apply[A: ShowPretty]: ShowPretty[A] = implicitly
}

@implicitNotFound("""Could not derive an instance of ShowPretty[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
trait MkShowPretty[A] extends ShowPretty[A]

object MkShowPretty extends MkShowPrettyDerivation {
  def apply[A](implicit ev: MkShowPretty[A]): MkShowPretty[A] = ev
}

sealed abstract private[derived] class MkShowPrettyDerivation extends MkShowPrettyGenericCoproduct {
  implicit val mkShowPrettyHNil: MkShowPretty[HNil] = _ => Nil
  implicit val mkShowPrettyCNil: MkShowPretty[CNil] = _ => Nil

  implicit def mkShowPrettyLabelledHCons[K <: Symbol, V, T <: HList](implicit
      K: Witness.Aux[K],
      V: Show[V] OrElse MkShowPretty[V],
      T: MkShowPretty[T]
  ): MkShowPretty[FieldType[K, V] :: T] = { case v :: t =>
    val name = K.value.name
    val valueLines = mkShowLines(V)(v)
    val middleLines = valueLines.drop(1)
    val tailLines = T.showLines(t)
    val middleEmpty = middleLines.isEmpty
    val tailEmpty = tailLines.isEmpty
    val value = valueLines.headOption.mkString
    val headLine = if (tailEmpty || !middleEmpty) s"$name = $value" else s"$name = $value,"

    if (tailEmpty) headLine :: middleLines
    else if (middleEmpty) headLine :: tailLines
    else headLine :: middleLines.init ::: s"${middleLines.last}," :: tailLines
  }

  implicit def mkShowPrettyCCons[L, R <: Coproduct](implicit
      L: Show[L] OrElse MkShowPretty[L],
      R: MkShowPretty[R]
  ): MkShowPretty[L :+: R] = {
    case Inl(l) => mkShowLines(L)(l)
    case Inr(r) => R.showLines(r)
  }

  implicit def mkShowPrettyProduct[A, R <: HList](implicit
      A: LabelledGeneric.Aux[A, R],
      T: ClassTag[A],
      R: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] = { a =>
    val name = T.runtimeClass.getSimpleName
    val lines = R.value.showLines(A.to(a)).map("  " + _)
    s"$name(" :: lines ::: ")" :: Nil
  }
}

sealed abstract private[derived] class MkShowPrettyGenericCoproduct {

  protected def mkShowLines[A](show: Show[A] OrElse MkShowPretty[A])(a: A): List[String] =
    show.fold(
      {
        case pretty: ShowPretty[A] => pretty.showLines(a)
        case other => other.show(a).split(System.lineSeparator).toList
      },
      _.showLines(a)
    )

  implicit def mkShowPrettyGenericCoproduct[A, R <: Coproduct](implicit
      A: Generic.Aux[A, R],
      R: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] = a => R.value.showLines(A.to(a))
}
