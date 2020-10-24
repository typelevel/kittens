package cats
package derived

import shapeless._
import shapeless.labelled._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

/**
 * Due to a limitation in the way Shapeless' `describe` is currently
 * implemented, `Show` can't be derived for ADTs which are _both_
 * recursive _and_ generic in one or more type parameters.
 *
 * See:
 * https://github.com/typelevel/kittens/pull/48#issue-249836267
 * https://github.com/milessabin/shapeless/issues/750
 *
 * See the test suite for more precise examples of what can and cannot
 * be derived.
 */
@implicitNotFound("""Could not derive an instance of Show[A] where A = ${A}.
Make sure that A has a Typeable instance and satisfies one of the following conditions:
  * it is a case class where all fields have a Show instance
  * it is a sealed trait where all subclasses have a Show instance""")
trait MkShow[A] extends Show[A]

object MkShow extends MkShowDerivation {
  def apply[A](implicit ev: MkShow[A]): MkShow[A] = ev
}

private[derived] abstract class MkShowDerivation extends MkShowGenericCoproduct {
  implicit val mkShowHNil: MkShow[HNil] = instance(_ => "")
  implicit val mkShowCNil: MkShow[CNil] = instance(_ => "")

  implicit def mkShowLabelledHCons[K <: Symbol, V, T <: HList](
    implicit K: Witness.Aux[K], V: Show[V] OrElse MkShow[V], T: MkShow[T]
  ): MkShow[FieldType[K, V] :: T] = instance { case v :: t =>
    val name = K.value.name
    val value = V.unify.show(v)
    val tail = T.show(t)
    if (tail.isEmpty) s"$name = $value"
    else s"$name = $value, $tail"
  }

  implicit def mkShowCCons[L, R <: Coproduct](
    implicit L: Show[L] OrElse MkShow[L], R: MkShow[R]
  ): MkShow[L :+: R] = instance {
    case Inl(l) => L.unify.show(l)
    case Inr(r) => R.show(r)
  }

  implicit def mkShowGenericProduct[A, R <: HList](
    implicit A: LabelledGeneric.Aux[A, R], T: Typeable[A], R: Lazy[MkShow[R]]
  ): MkShow[A] = instance { a =>
    val name = T.describe.takeWhile(_ != '[')
    val fields = R.value.show(A.to(a))
    s"$name($fields)"
  }
}

private[derived] abstract class MkShowGenericCoproduct {

  implicit def mkShowGenericCoproduct[A, R <: Coproduct](
    implicit A: Generic.Aux[A, R], R: Lazy[MkShow[R]]
  ): MkShow[A] = instance(a => R.value.show(A.to(a)))

  protected def instance[A](f: A => String): MkShow[A] =
    new MkShow[A] {
      def show(value: A): String = f(value)
    }
}
