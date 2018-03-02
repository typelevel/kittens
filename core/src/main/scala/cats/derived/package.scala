package cats

import alleycats._
import shapeless.{Cached, Refute}

/** Full auto derivation of type classes. */
package object derived {


  object iterable extends IterableDerivationFromMkIterable

  object pure extends MkPureDerivation

  @deprecated("use cats.derive.semigroup instead", "1.0.0-RC1")
  object semigroup extends MkSemigroupDerivation

  @deprecated("use cats.derive.semigroupK instead", "1.0.0-RC1")
  object semigroupK extends MkSemigroupK0

}


package derived {

  /**
    * Fully automatically derive the instance, note that this derivation is not cached, so it
    * will re-derive every time an instance is needed in the application, which could
    * significantly impact the compilation time.
    */
  object auto {


    object empty {
      implicit def kittensMkEmpty[A](
                             implicit refute: Refute[Empty[A]], ev: MkEmpty[A]
                           ): Empty[A] = ev

    }

    object emptyK extends MkEmptyKDerivation

    object eq {
      implicit def kittensMkEq[A](
                                   implicit refute: Refute[Eq[A]], eq: MkEq[A]
                                 ): Eq[A] = eq
    }


    object functor {
      implicit def kittensMkFunctor[F[_]](
                                  implicit refute: Refute[Functor[F]], ev: MkFunctor[F]
                                ): Functor[F] = ev

    }

    object show {
      implicit def kittensMkShow[A](
                                     implicit refute: Refute[Show[A]], show: MkShow[A]
                                   ): Show[A] = show
    }

    object monoid extends MkMonoidDerivation

    object monoidK {
      implicit def kittensMkMonoidK[F[_]](
                                           implicit refute: Refute[MonoidK[F]], ev: MkMonoidK[F]
                                         ): MonoidK[F] = ev
    } 

    object foldable extends MkFoldableDerivation


  }

  /**
    * cached cache the derived instance but this cache are global, so be cautious only use it
    * when there is only one instance globally in your application.
    */
  object cached {

    object empty {
      implicit def kittensMkEmpty[A](implicit refute: Refute[Empty[A]], ev: Cached[MkEmpty[A]])
      : Empty[A] = ev.value
    }

    object eq {
      implicit def kittensMkEq[A](implicit refute: Refute[Eq[A]], ev: Cached[MkEq[A]])
      : Eq[A] = ev.value

    }

    object functor {
      implicit def kittensMkFunctor[F[_]](implicit refute: Refute[Functor[F]], ev: Cached[MkFunctor[F]])
      : Functor[F] = ev.value
    }

    object show {
      implicit def kittensMkshow[A](implicit refute: Refute[Show[A]], ev: Cached[MkShow[A]])
      : Show[A] = ev.value
    }

    object monoidK {
      implicit def kittensMkMonoidK[F[_]](
                                           implicit refute: Refute[MonoidK[F]], ev: Cached[MkMonoidK[F]]
                                         ): MonoidK[F] = ev.value
    }

  }

  /**
    * allows semi automatically derive each instance. The derivation might need help when
    * there are fields with type constructor that comes with instances
    * e.g.
    * {{{
    * scala> case class Foo(bars: List[Bar])
    * scala> case class Bar(a: String)
    *
    * scala> cats.derived.semi.show[Foo].show(Foo(List(Bar("a"))))
    * res1: String = Foo(bars = $colon$colon(head = Bar(a = a), tl$access$1 = Nil.type()))
    * }}}
    * Note that semi.show didn't respect the native `Show[List]` instance
    *
    * You could either derive a Bar instance first
    * {{{
    * scala> implicit val barShow = cats.derived.semi.show[Bar]
    *
    * scala> cats.derived.semi.show[Foo].show(Foo(List(Bar("a"))))
    * res2: String = Foo(bars = List(Bar(a = a)))
    * }}}
    *
    * Or you can take advantage of a controlled auto derivation
    * {{{
    *   scala> implicit val fooShow: Show[Foo] = { |
    *             import cats.derived.auto.show._  |
    *             cats.derived.semi.show           |
    *          }
    *  scala> Foo(List(Bar("a"))).show
    *  res3: String = Foo(bars = List(Bar(a = a)))
    * }}}
    */
  object semi {

    def empty[A](implicit ev: MkEmpty[A]): Empty[A] = ev

    def emptyK[F[_]](implicit F: MkEmptyK[F]): EmptyK[F] = F

    def eq[A](implicit ev: MkEq[A]): Eq[A] = ev

    def functor[F[_]](implicit ev: MkFunctor[F]): Functor[F] = ev

    def show[A](implicit ev: MkShow[A]): Show[A] = ev

    def foldable[F[_]](implicit F: MkFoldable[F]): Foldable[F] = F

    def monoid[T](implicit T: MkMonoid[T]): Monoid[T] = T

    def monoidK[F[_]](implicit F: MkMonoidK[F]): MonoidK[F] = F

    def pure[F[_]](implicit F: MkPure[F]): Pure[F] = F

    def semigroup[T](implicit F: MkSemigroup[T]): Semigroup[T] = F

    def semigroupK[F[_]](implicit F: MkSemigroupK[F]): SemigroupK[F] = F

    def iterable[F[_], A](fa: F[A])(implicit mif: MkIterable[F]): Iterable[A] = mif.iterable(fa)
  }

}
