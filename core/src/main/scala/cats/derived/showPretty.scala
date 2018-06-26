package cats.derived

import cats.Show
import shapeless._
import shapeless.labelled._

trait ShowPretty[A] extends Show[A] {
  def showLines(a: A): List[String]
  def show(a: A): String = showLines(a).mkString("\n")
}

object ShowPretty {
  implicit def fromShow[A](implicit s: Show[A]): ShowPretty[A] =
    new ShowPretty[A] {
      override def showLines(a: A): List[String] =
        s.show(a).split("\\n").toList
    }
}

trait MkShowPretty[A] extends ShowPretty[A]

object MkShowPretty extends MkShowPrettyDerivation {
  def apply[A](implicit showPretty: MkShowPretty[A]): MkShowPretty[A] =
    showPretty
}

trait MkShowPrettyDerivation extends MkShowPretty1 {
  implicit val emptyProductDerivedShowPretty: MkShowPretty[HNil] =
    instance(_ => Nil)

  implicit def productDerivedShowPretty[K <: Symbol, V, T <: HList](
      implicit key: Witness.Aux[K],
      showV: ShowPretty[V] OrElse MkShowPretty[V],
      showT: MkShowPretty[T]
  ): MkShowPretty[FieldType[K, V] :: T] = instance { fields =>
    val fieldName = key.value.name
    val fieldValueLines = showV.unify.showLines(fields.head)
    val nextFields = showT.showLines(fields.tail)

    val fieldValue = {
      val head = fieldValueLines.headOption.mkString
      if (nextFields.isEmpty || fieldValueLines.size > 1) head
      else s"$head,"
    }

    val remainingLines =
      if (fieldValueLines.size > 1) {
        val tail = fieldValueLines.tail
        if (nextFields.isEmpty) tail
        else tail.init ++ tail.lastOption.map(s => s"$s,")
      } else Nil

    List(s"$fieldName = $fieldValue") ++ remainingLines ++ nextFields
  }

  implicit def emptyCoproductDerivedShowPretty: MkShowPretty[CNil] =
    instance(_ => Nil)
}

trait MkShowPretty1 extends MkShowPretty2 {
  implicit def coproductDerivedShowPretty[K <: Symbol, V, T <: Coproduct](
      implicit key: Witness.Aux[K],
      showV: ShowPretty[V] OrElse MkShowPretty[V],
      showT: MkShowPretty[T]
  ): MkShowPretty[FieldType[K, V] :+: T] = instance {
    case Inl(l) => showV.unify.showLines(l)
    case Inr(r) => showT.showLines(r)
  }
}

trait MkShowPretty2 extends MkShowPretty3 {
  implicit def genericDerivedShowPrettyProduct[A, R <: HList](
      implicit repr: LabelledGeneric.Aux[A, R],
      t: Typeable[A],
      s: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] = instance { a =>
    val name = t.describe.takeWhile(_ != '[')
    val contentLines = s.value.showLines(repr.to(a))
    val contents = contentLines.map(s => s"  $s")
    List(s"$name(") ++ contents ++ List(")")
  }
}

trait MkShowPretty3 {
  protected def instance[A](f: A => List[String]): MkShowPretty[A] =
    new MkShowPretty[A] {
      def showLines(a: A): List[String] = f(a)
    }

  implicit def genericDerivedShowPrettyCoproduct[A, R <: Coproduct](
      implicit repr: LabelledGeneric.Aux[A, R],
      s: Lazy[MkShowPretty[R]]
  ): MkShowPretty[A] =
    instance(a => s.value.showLines(repr.to(a)))
}
