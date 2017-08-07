package cats.derived

import cats.Show
import export.reexports
import shapeless._


@reexports[MkShow]
object show

trait MkShow[A] extends Show[A]

object MkShow extends LabelledTypeClassCompanion[Show] {

  def apply[T](implicit e: MkShow[T]): MkShow[T] = e

  implicit object typeClass extends LabelledTypeClass[Show] {
    def emptyProduct: Show[HNil] = new Show[HNil] {
      def show(t: HNil) = ""
    }

    def product[F, T <: HList](name: String, sh: Show[F], st: Show[T]): Show[F :: T] = new Show[F :: T] {
      def show(ft: F :: T): String = {
        val head = sh.show(ft.head)
        val tail = st.show(ft.tail)
        if (tail.isEmpty)
          s"$name = $head"
        else
          s"$name = $head, $tail"
      }
    }

    def emptyCoproduct: Show[CNil] = ???

    def coproduct[L, R <: Coproduct](name: String, sl: => Show[L], sr: => Show[R]): Show[L :+: R] = new Show[L :+: R]{
      def show(lr: L :+: R): String = lr match {
        case Inl(l) => s"$name(${sl.show(l)})"
        case Inr(r) => s"${sr.show(r)}"
      }
    }

    def project[F, G](instance: => Show[G], to: F => G, from: G => F): Show[F] = new Show[F] {
      def show(f: F) = instance.show(to(f))
    }
  }
}
