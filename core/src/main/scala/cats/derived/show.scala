package cats.derived

import cats.Show
import export.{ exports, reexports }
import shapeless._, labelled._

@reexports[MkShow]
object show

/**
 * Due to a limitation in the way Shapeless' `describe` is currently
 * implemented, `Show` can't be derived for ADTs which are _both_
 * recursive _and_ generic in one or more type parameters.
 *
 * See:
 * https://github.com/milessabin/kittens/pull/48#issue-249836267
 * https://github.com/milessabin/shapeless/issues/750
 *
 * See the test suite for more precise examples of what can and cannot
 * be derived.
 */
trait MkShow[A] extends Show[A]

@exports
object MkShow {
  private def instance[A](body: A => String) = new Show[A] {
    def show(value: A): String = body(value)
  }

  implicit def emptyProductDerivedShow: Show[HNil] =
    instance(_ => "")

  implicit def productDerivedShow[K <: Symbol, V, T <: HList](
      implicit key: Witness.Aux[K],
      showV: Lazy[Show[V]],
      showT: Show[T]): Show[FieldType[K, V] :: T] = instance { fields =>
    val fieldName = key.value.name
    val fieldValue = showV.value.show(fields.head)
    val nextFields = showT.show(fields.tail)

    if (nextFields.isEmpty)
      s"$fieldName = $fieldValue"
    else
      s"$fieldName = $fieldValue, $nextFields"
  }

  implicit def emptyCoproductDerivedShow: Show[CNil] =
    sys.error("Kittens derived Show instance: impossible to call `show` on `CNil`")

  implicit def coproductDerivedShow[K <: Symbol, V, T <: Coproduct](
      implicit key: Witness.Aux[K],
      showV: Lazy[Show[V]],
      showT: Lazy[Show[T]]): Show[FieldType[K, V] :+: T] = instance {
    case Inl(l) => showV.value.show(l)
    case Inr(r) => showT.value.show(r)
  }

  implicit def genericDerivedShowProduct[A, R <: HList](
      implicit repr: LabelledGeneric.Aux[A, R],
      t: Lazy[Typeable[A]],
      s: Lazy[Show[R]]): Show[A] = instance { a =>
    val name = t.value.describe.takeWhile(_ != '[')
    val contents = s.value.show(repr.to(a))

    s"$name($contents)"
  }

  implicit def genericDerivedShowCoproduct[A, R <: Coproduct](
      implicit repr: LabelledGeneric.Aux[A, R],
      s: Lazy[Show[R]]): Show[A] =
    instance(a => s.value.show(repr.to(a)))
}
