package cats
package derived

import alleycats.{ConsK, Empty, EmptyK, Pure}
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

/**
 * allows semi automatically derive each instance. The derivation might need help when
 * there are fields with a type constructor that comes with instances, e.g.
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
 *          }
 *  scala> Foo(List(Bar("a"))).show
 *  res3: String = Foo(bars = List(Bar(a = a)))
 * }}}
 */
object semiauto {

  def eq[A](implicit ev: MkEq[A]): Eq[A] = ev
  def partialOrder[A](implicit ev: MkPartialOrder[A]): PartialOrder[A] = ev
  def order[A](implicit ev: MkOrder[A]): Order[A] = ev
  def hash[A](implicit ev: MkHash[A]): Hash[A] = ev

  def show[A](implicit ev: MkShow[A]): Show[A] = ev
  def showPretty[A](implicit ev: MkShowPretty[A]): ShowPretty[A] = ev

  def empty[A](implicit ev: MkEmpty[A]): Empty[A] = ev
  def emptyK[F[_]](implicit F: MkEmptyK[F]): EmptyK[F] = F

  def semigroup[T](implicit ev: MkSemigroup[T]): Semigroup[T] = ev
  def semigroupK[F[_]](implicit F: MkSemigroupK[F]): SemigroupK[F] = F
  def commutativeSemigroup[T](implicit ev: MkCommutativeSemigroup[T]): CommutativeSemigroup[T] = ev

  def monoid[A](implicit ev: MkMonoid[A]): Monoid[A] = ev
  def monoidK[F[_]](implicit F: MkMonoidK[F]): MonoidK[F] = F
  def commutativeMonoid[A](implicit ev: MkCommutativeMonoid[A]): CommutativeMonoid[A] = ev

  def functor[F[_]](implicit F: MkFunctor[F]): Functor[F] = F
  def contravariant[F[_]](implicit F: MkContravariant[F]): Contravariant[F] = F
  def invariant[F[_]](implicit F: MkInvariant[F]): Invariant[F] = F
  def pure[F[_]](implicit F: MkPure[F]): Pure[F] = F
  def apply[F[_]](implicit F: MkApply[F]): Apply[F] = F
  def applicative[F[_]](implicit F: MkApplicative[F]): Applicative[F] = F

  def foldable[F[_]](implicit F: MkFoldable[F]): Foldable[F] = F
  def reducible[F[_]](implicit F: MkReducible[F]): Reducible[F] = F
  def traverse[F[_]](implicit F: MkTraverse[F]): Traverse[F] = F
  def nonEmptyTraverse[F[_]](implicit F: MkNonEmptyTraverse[F]): NonEmptyTraverse[F] = F

  def consK[F[_]](implicit F: MkConsK[F, F]): ConsK[F] = MkConsK.consK(F)
  def iterable[F[_], A](fa: F[A])(implicit F: MkIterable[F]): Iterable[A] = F.iterable(fa)
}
