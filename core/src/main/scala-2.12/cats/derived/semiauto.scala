package cats
package derived

import alleycats._
import cats.derived.util.VersionSpecific.Lazy
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

/** allows semi automatically derive each instance. The derivation might need help when there are fields with a type
  * constructor that comes with instances, e.g.
  * {{{
  * scala> case class Bar(a: String)
  * scala> case class Foo(bars: List[Bar])
  * scala> import cats.instances.all._
  *
  * scala> cats.derived.semiauto.show[Foo].show(Foo(List(Bar("a"))))
  * res1: String = Foo(bars = \$colon\$colon(head = Bar(a = a), tl\$access\$1 = Nil.type()))
  * }}}
  * Note that semi.show didn't respect the native `Show[List]` instance
  *
  * You could either derive a Bar instance first
  * {{{
  * scala> implicit val barShow = cats.derived.semi.show[Bar]
  *
  * scala> cats.derived.semiauto.show[Foo].show(Foo(List(Bar("a"))))
  * res2: String = Foo(bars = List(Bar(a = a)))
  * }}}
  *
  * Or you can take advantage of a controlled auto derivation
  * {{{
  *   scala> implicit val fooShow: Show[Foo] = { |
  *             import cats.derived.auto.show._  |
  *             cats.derived.semiauto.show       |
  *           }
  *   scala> Foo(List(Bar("a"))).show
  *   res3: String = Foo(bars = List(Bar(a = a)))
  * }}}
  */
object semiauto {

  def eq[A](implicit ev: Lazy[MkEq[A]]): Eq[A] = ev.value
  def partialOrder[A](implicit ev: Lazy[MkPartialOrder[A]]): PartialOrder[A] = ev.value
  def order[A](implicit ev: Lazy[MkOrder[A]]): Order[A] = ev.value
  def hash[A](implicit ev: Lazy[MkHash[A]]): Hash[A] = ev.value

  def show[A](implicit ev: Lazy[MkShow[A]]): Show[A] = ev.value
  def showPretty[A](implicit ev: Lazy[MkShowPretty[A]]): ShowPretty[A] = ev.value

  def empty[A](implicit ev: Lazy[MkEmpty[A]]): Empty[A] = ev.value
  def emptyK[F[_]](implicit F: Lazy[MkEmptyK[F]]): EmptyK[F] = F.value

  def semigroup[T](implicit ev: Lazy[MkSemigroup[T]]): Semigroup[T] = ev.value
  def semigroupK[F[_]](implicit F: Lazy[MkSemigroupK[F]]): SemigroupK[F] = F.value
  def commutativeSemigroup[T](implicit ev: Lazy[MkCommutativeSemigroup[T]]): CommutativeSemigroup[T] = ev.value

  def monoid[A](implicit ev: Lazy[MkMonoid[A]]): Monoid[A] = ev.value
  def monoidK[F[_]](implicit F: Lazy[MkMonoidK[F]]): MonoidK[F] = F.value
  def commutativeMonoid[A](implicit ev: Lazy[MkCommutativeMonoid[A]]): CommutativeMonoid[A] = ev.value

  def functor[F[_]](implicit F: Lazy[MkFunctor[F]]): Functor[F] = F.value
  def contravariant[F[_]](implicit F: Lazy[MkContravariant[F]]): Contravariant[F] = F.value
  def invariant[F[_]](implicit F: Lazy[MkInvariant[F]]): Invariant[F] = F.value
  def pure[F[_]](implicit F: Lazy[MkPure[F]]): Pure[F] = F.value
  def apply[F[_]](implicit F: Lazy[MkApply[F]]): Apply[F] = F.value
  def applicative[F[_]](implicit F: Lazy[MkApplicative[F]]): Applicative[F] = F.value

  def foldable[F[_]](implicit F: Lazy[MkFoldable[F]]): Foldable[F] = F.value
  def reducible[F[_]](implicit F: Lazy[MkReducible[F]]): Reducible[F] = F.value
  def traverse[F[_]](implicit F: Lazy[MkTraverse[F]]): Traverse[F] = F.value
  def nonEmptyTraverse[F[_]](implicit F: Lazy[MkNonEmptyTraverse[F]]): NonEmptyTraverse[F] = F.value

  def consK[F[_]](implicit F: Lazy[MkConsK[F, F]]): ConsK[F] = MkConsK.consK(F.value)
  def iterable[F[_], A](fa: F[A])(implicit F: MkIterable[F]): Iterable[A] = F.iterable(fa)
}
