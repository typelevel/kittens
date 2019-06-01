package cats
package derived

import alleycats._
import shapeless.{Cached, Lazy, Refute}

/**
  * Fully automatically derive the instance, note that this derivation is not cached, so it
  * will re-derive every time an instance is needed in the application, which could
  * significantly impact the compilation time.
  */
object auto {

  object iterable extends IterableDerivationFromMkIterable

  object empty {
    implicit def kittensMkEmpty[A](
      implicit refute: Refute[Empty[A]], ev: Lazy[MkEmpty[A]]
    ): Empty[A] = ev.value

  }

  object emptyK {
    implicit def kittensMkEmptyK[F[_]](
      implicit refute: Refute[EmptyK[F]], F: Lazy[MkEmptyK[F]]
    ): EmptyK[F] = F.value
  }

  object eq {
    implicit def kittensMkEq[A](
      implicit refute: Refute[Eq[A]], eq: MkEq[A]
    ): Eq[A] = eq
  }

  object partialOrder {
    implicit def kittensMkPartialOrder[A](
      implicit refute: Refute[PartialOrder[A]], ev: Lazy[MkPartialOrder[A]]
    ): PartialOrder[A] = ev.value
  }

  object order {
    implicit def kittensMkOrder[A](
      implicit refute: Refute[Order[A]], ord: MkOrder[A]
    ): Order[A] = ord
  }

  object hash {
    implicit def kittensMkHash[A](
      implicit refute: Refute[Hash[A]], hash: MkHash[A]
    ): Hash[A] = hash
  }

  object functor {
    implicit def kittensMkFunctor[F[_]](
      implicit refute: Refute[Functor[F]], F: Lazy[MkFunctor[F]]
    ): Functor[F] = F.value
  }

  object apply {
    implicit def kittensMkApply[F[_]](
      implicit refute: Refute[Apply[F]], F: Lazy[MkApply[F]]
    ): Apply[F] = F.value
  }

  object applicative {
    implicit def kittensMkApplicative[F[_]](
      implicit refute: Refute[Applicative[F]], F: Lazy[MkApplicative[F]]
    ): Applicative[F] = F.value
  }

  object show {
    implicit def kittensMkShow[A](
      implicit refute: Refute[Show[A]], show: MkShow[A]
    ): Show[A] = show
  }

  object showPretty {
    implicit def kittensMkShowPretty[A](
     implicit refute: Refute[Show[A]], showPretty: MkShowPretty[A]
    ): ShowPretty[A] = showPretty
  }

  object semigroup {
    implicit def kittensMkSemigroup[A](
      implicit refute: Refute[Semigroup[A]], ev: Lazy[MkSemigroup[A]]
    ): Semigroup[A] = ev.value
  }

  object monoid {
    implicit def kittensMkMonoid[A](
      implicit refute: Refute[Monoid[A]], ev: Lazy[MkMonoid[A]]
    ): Monoid[A] = ev.value
  }

  object semigroupK {
    implicit def kittensMkSemigroupK[F[_]](
      implicit refute: Refute[SemigroupK[F]], F: Lazy[MkSemigroupK[F]]
    ): SemigroupK[F] = F.value
  }

  object monoidK {
    implicit def kittensMkMonoidK[F[_]](
      implicit refute: Refute[MonoidK[F]], F: Lazy[MkMonoidK[F]]
    ): MonoidK[F] = F.value
  }

  object foldable {
    implicit def kittensMkFoldable[F[_]](
      implicit refute: Refute[Foldable[F]], F: Lazy[MkFoldable[F]]
    ): Foldable[F] = F.value
  }

  object traverse {
    implicit def kittensMkTraverse[F[_]](
      implicit refute: Refute[Traverse[F]], F: Lazy[MkTraverse[F]]
    ): Traverse[F] = F.value
  }

  object pure {
    implicit def kittensMkPure[F[_]](
      implicit refute: Refute[Pure[F]], F: Lazy[MkPure[F]]
    ): Pure[F] = F.value
  }

  object consK {
    implicit def kittensMkConsK[F[_]](
      implicit refute: Refute[ConsK[F]], F: Lazy[MkConsK[F, F]]
    ): ConsK[F] = MkConsK.consK(F.value)
  }
}

/**
  * cached cache the derived instance but this cache are global, so be cautious only use it
  * when there is only one instance globally in your application.
  */
object cached {

  object empty {
    implicit def kittensMkEmpty[A](
      implicit refute: Refute[Empty[A]], cached: Cached[MkEmpty[A]]
    ): Empty[A] = cached.value
  }

  object emptyK {
    implicit def kittensMkEmptyK[F[_]](
      implicit refute: Refute[EmptyK[F]], cached: Cached[MkEmptyK[F]]
    ): EmptyK[F] = cached.value
  }

  object pure {
    implicit def kittensMkPure[F[_]](
      implicit refute: Refute[Pure[F]], cached: Cached[MkPure[F]]
    ): Pure[F] = cached.value
  }

  object eq {
    implicit def kittensMkEq[A](
      implicit refute: Refute[Eq[A]], ev: Cached[MkEq[A]])
    : Eq[A] = ev.value

  }

  object partialOrder {
    implicit def kittensMkPartialOrder[A](
      implicit refute: Refute[PartialOrder[A]], cached: Cached[MkPartialOrder[A]]
    ): PartialOrder[A] = cached.value
  }

  object order {
    implicit def kittensMkOrder[A](
       implicit refute: Refute[Order[A]], ord: Cached[MkOrder[A]])
    : Order[A] = ord.value
  }

  object hash {
    implicit def kittensMkHash[A](
       implicit refute: Refute[Hash[A]], ord: Cached[MkHash[A]])
    : Hash[A] = ord.value
  }

  object functor {
    implicit def kittensMkFunctor[F[_]](
      implicit refute: Refute[Functor[F]], cached: Cached[MkFunctor[F]]
    ): Functor[F] = cached.value
  }

  object apply {
    implicit def kittensMkApply[F[_]](
      implicit refute: Refute[Apply[F]], cached: Cached[MkApply[F]]
    ): Apply[F] = cached.value
  }

  object applicative {
    implicit def kittensMkApplicative[F[_]](
      implicit refute: Refute[Applicative[F]], cached: Cached[MkApplicative[F]]
    ): Applicative[F] = cached.value
  }

  object foldable {
    implicit def kittensMkFoldable[F[_]](
       implicit refute: Refute[Foldable[F]], cached: Cached[MkFoldable[F]]
    ): Foldable[F] = cached.value
  }

  object traverse{
    implicit def kittensMkTraverse[F[_]](
       implicit refute: Refute[Traverse[F]], cached: Cached[MkTraverse[F]]
    ): Traverse[F] = cached.value
  }

  object show {
    implicit def kittensMkshow[A](
      implicit refute: Refute[Show[A]], ev: Cached[MkShow[A]])
    : Show[A] = ev.value
  }

  object showPretty {
    implicit def kittensMkShowPretty[A](
      implicit refute: Refute[Show[A]], ev: Cached[MkShowPretty[A]])
    : ShowPretty[A] = ev.value
  }

  object monoidK {
    implicit def kittensMkMonoidK[F[_]](
      implicit refute: Refute[MonoidK[F]], cached: Cached[MkMonoidK[F]]
    ): MonoidK[F] = cached.value
  }

  object semigroup {
    implicit def kittensMkSemigroup[A](
      implicit refute: Refute[Semigroup[A]], cached: Cached[MkSemigroup[A]]
    ): Semigroup[A] = cached.value
  }

  object monoid {
    implicit def kittensMkMonoid[A](
      implicit refute: Refute[Monoid[A]], cached: Cached[MkMonoid[A]]
    ): Monoid[A] = cached.value
  }

  object semigroupK {
    implicit def kittensMkSemigroupK[F[_]](
      implicit refute: Refute[SemigroupK[F]], cached: Cached[MkSemigroupK[F]]
    ): SemigroupK[F] = cached.value
  }

  object consK {
    implicit def kittensMkConsK[F[_]](
      implicit refute: Refute[ConsK[F]], cached: Cached[MkConsK[F, F]]
    ): ConsK[F] = MkConsK.consK(cached.value)
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

  def empty[A](implicit ev: Lazy[MkEmpty[A]]): Empty[A] = ev.value

  def emptyK[F[_]](implicit F: Lazy[MkEmptyK[F]]): EmptyK[F] = F.value

  def eq[A](implicit ev: MkEq[A]): Eq[A] = ev

  def partialOrder[A](implicit ev: Lazy[MkPartialOrder[A]]): PartialOrder[A] = ev.value

  def order[A](implicit ev: MkOrder[A]): Order[A] = ev

  def hash[A](implicit ev: MkHash[A]): Hash[A] = ev

  def functor[F[_]](implicit F: Lazy[MkFunctor[F]]): Functor[F] = F.value

  def apply[F[_]](implicit F: Lazy[MkApply[F]]): Apply[F] = F.value

  def applicative[F[_]](implicit F: Lazy[MkApplicative[F]]): Applicative[F] = F.value

  def show[A](implicit ev: MkShow[A]): Show[A] = ev

  def showPretty[A](implicit ev: MkShowPretty[A]): ShowPretty[A] = ev

  def foldable[F[_]](implicit F: Lazy[MkFoldable[F]]): Foldable[F] = F.value

  def traverse[F[_]](implicit F: Lazy[MkTraverse[F]]): Traverse[F] = F.value

  def monoid[A](implicit ev: Lazy[MkMonoid[A]]): Monoid[A] = ev.value

  def monoidK[F[_]](implicit F: Lazy[MkMonoidK[F]]): MonoidK[F] = F.value

  def pure[F[_]](implicit F: Lazy[MkPure[F]]): Pure[F] = F.value

  def semigroup[T](implicit ev: Lazy[MkSemigroup[T]]): Semigroup[T] = ev.value

  def semigroupK[F[_]](implicit F: Lazy[MkSemigroupK[F]]): SemigroupK[F] = F.value

  def consK[F[_]](implicit F: Lazy[MkConsK[F, F]]): ConsK[F] = MkConsK.consK(F.value)

  def iterable[F[_], A](fa: F[A])(implicit mif: MkIterable[F]): Iterable[A] = mif.iterable(fa)
}

