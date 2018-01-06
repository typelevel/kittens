package cats
package derived

import shapeless._
import shapeless.labelled.FieldType
import scala.annotation.implicitNotFound


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
@implicitNotFound("Could not derive an instance of Show[${A}]")
class MkShow[A](val instance: Show[A])

object MkShow extends MkShowDerivation {
  def apply[A](implicit show: MkShow[A]): MkShow[A] = show
}

private[derived] abstract class MkShowDerivation
  extends MkShow0 with Prioritized[MkShow] {

  protected def derived[A](f: A => String): Derived[A] =
    new MkShow[A](Show.show(f)) with Priority.Derived

  implicit def fromShow[A](implicit show: Show[A]): Hidden[A] =
    new MkShow[A](show) with Priority.Hidden
}

private[derived] abstract class MkShow0 {
  this: MkShowDerivation =>

  implicit val emptyProductDerivedShow: Derived[HNil] =
    derived(_ => "")

  implicit val emptyCoproductDerivedShow: Derived[CNil] =
    derived(_ => unexpected)

  implicit def fieldTypeDerivedShow[K <: Symbol, V](
    implicit K: Witness.Aux[K], V: MkShow[V]
  ): Derived[FieldType[K, V]] = derived { field =>
    val name = K.value.name
    val value = V.instance.show(field)
    s"$name = $value"
  }

  implicit def productDerivedShow[H, T <: HList](
     implicit H: Lazy[MkShow[H]], T: Derived[T]
  ): Derived[H :: T] = derived { case h :: t =>
    val head = H.value.instance.show(h)
    val tail = T.instance.show(t)
    if (tail.isEmpty) head else s"$head, $tail"
  }

  implicit def coproductDerivedShow[L, R <: Coproduct](
    implicit L: Lazy[MkShow[L]], R: Derived[R]
  ): Derived[L :+: R] = derived {
    case Inl(l) => L.value.instance.show(l)
    case Inr(r) => R.instance.show(r)
  }

  implicit def genericProductDerivedShow[A, R <: HList](
    implicit gen: LabelledGeneric.Aux[A, R], typ: Typeable[A], R: Derived[R]
  ): Derived[A] = derived { a =>
    val name = typ.describe.takeWhile(_ != '[')
    val fields = R.instance.show(gen.to(a))
    s"$name($fields)"
  }

  implicit def genericCoproductDerivedShow[A, R <: Coproduct](
    implicit gen: Generic.Aux[A, R], R: Derived[R]
  ): Derived[A] = derived { a =>
    R.instance.show(gen.to(a))
  }
}
