package cats.derived

import cats.Show
import shapeless._
import shapeless.labelled.FieldType

trait ShowPretty[A] extends Show[A] {
  def showWithIndent(a: A, indent: Int): String
  def show(a: A): String = showWithIndent(a, 0)
}

object ShowPretty {
  implicit def fromShow[A](implicit s: Show[A]): ShowPretty[A] =
    new ShowPretty[A] {
      override def showWithIndent(a: A, indent: Int): String =
        s.show(a)
    }
}

trait MkShowPretty[A] extends ShowPretty[A]

object MkShowPretty extends MkShowPrettyDerivation {
  def apply[A](implicit showPretty: MkShowPretty[A]): MkShowPretty[A] =
    showPretty
}

trait MkShowPrettyDerivation extends MkShowPretty1 {
  implicit val emptyProductDerivedShowPretty: MkShowPretty[HNil] =
    instance((_, _) => "")

  implicit def productDerivedShowPretty[K <: Symbol, V, T <: HList](
      implicit key: Witness.Aux[K],
      showV: ShowPretty[V] OrElse MkShowPretty[V],
      showT: MkShowPretty[T]
  ): MkShowPretty[FieldType[K, V] :: T] = instance { (fields, indent) =>
    val fieldName = key.value.name
    val fieldValue = showV.unify.showWithIndent(fields.head, indent)
    val nextFields = showT.showWithIndent(fields.tail, indent)

    if (nextFields.isEmpty)
      s"${" " * indent}$fieldName = $fieldValue"
    else
      s"${" " * indent}$fieldName = $fieldValue,\n$nextFields"
  }

  implicit def emptyCoproductDerivedShowPretty: MkShowPretty[CNil] =
    instance((_, _) => "")
}

trait MkShowPretty1 extends MkShowPretty2 {
  implicit def coproductDerivedShowPretty[K <: Symbol, V, T <: Coproduct](
      implicit key: Witness.Aux[K],
      showV: ShowPretty[V] OrElse MkShowPretty[V],
      showT: MkShowPretty[T]
  ): MkShowPretty[FieldType[K, V] :+: T] = instance {
    case (Inl(l), indent) => showV.unify.showWithIndent(l, indent)
    case (Inr(r), indent) => showT.showWithIndent(r, indent)
  }
}

trait MkShowPretty2 extends MkShowPretty3 {
  implicit def genericDerivedShowPrettyProduct[A, R <: HList](
      implicit repr: LabelledGeneric.Aux[A, R],
      t: Typeable[A],
      s: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] = instance { (a, indent) =>
    val name = t.describe.takeWhile(_ != '[')
    val contents = s.value.showWithIndent(repr.to(a), indent + 2)
    s"$name(\n$contents\n${" " * indent})"
  }
}

trait MkShowPretty3 {
  protected def instance[A](f: (A, Int) => String): MkShowPretty[A] =
    new MkShowPretty[A] {
      def showWithIndent(a: A, indent: Int): String = f(a, indent)
    }

  implicit def genericDerivedShowPrettyCoproduct[A, R <: Coproduct](
      implicit repr: LabelledGeneric.Aux[A, R],
      s: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] =
    instance((a, indent) => s.value.showWithIndent(repr.to(a), indent))
}
