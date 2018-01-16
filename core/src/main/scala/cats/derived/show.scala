package cats.derived

import cats.Show
import shapeless._, labelled._

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

object MkShow extends MkShowDerivation {
  def apply[A](implicit show: MkShow[A]): MkShow[A] = show
}

trait MkShowDerivation extends MkShow1 {
  implicit val emptyProductDerivedShow: MkShow[HNil] =
    instance(_ => "")

  implicit def productDerivedShowFurther[K <: Symbol, V, T <: HList](
       implicit key: Witness.Aux[K],
       showV: Show[V] OrElse MkShow[V],
       showT: MkShow[T]): MkShow[FieldType[K, V] :: T] = instance { fields =>
    val fieldName = key.value.name
    val fieldValue = showV.unify.show(fields.head)
    val nextFields = showT.show(fields.tail)

    if (nextFields.isEmpty)
      s"$fieldName = $fieldValue"
    else
      s"$fieldName = $fieldValue, $nextFields"
  }


  implicit def emptyCoproductDerivedShow: MkShow[CNil] =
    instance(_ => "")

}

trait MkShow1 extends MkShow2 {
  // used when Show[V] (a member of the coproduct) has to be derived.
  implicit def coproductDerivedShow[K <: Symbol, V, T <: Coproduct](
     implicit key: Witness.Aux[K],
     showV: Show[V] OrElse MkShow[V],
     showT: MkShow[T]): MkShow[FieldType[K, V] :+: T] = instance {
    case Inl(l) => showV.unify.show(l)
    case Inr(r) => showT.show(r)
  }

}

trait MkShow2 extends MkShow3 {


  implicit def genericDerivedShowProduct[A, R <: HList](
                                                         implicit repr: LabelledGeneric.Aux[A, R],
                                                         t: Typeable[A],
                                                         s: Lazy[MkShow[R]]): MkShow[A] = instance { a =>
    val name = t.describe.takeWhile(_ != '[')
    val contents = s.value.show(repr.to(a))

    s"$name($contents)"
  }
}

trait MkShow3 {
  protected def instance[A](body: A => String): MkShow[A] = new MkShow[A] {
    def show(value: A): String = body(value)
  }

  implicit def genericDerivedShowCoproduct[A, R <: Coproduct](
      implicit repr: LabelledGeneric.Aux[A, R],
      s: Lazy[MkShow[R]]): MkShow[A] =
    instance(a => s.value.show(repr.to(a)))
}
