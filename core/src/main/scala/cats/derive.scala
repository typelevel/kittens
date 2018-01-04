package cats

import alleycats._
import cats.derived._
import shapeless.Lazy


object derive {
  def functor[F[_]](implicit F: Lazy[MkFunctor.LowPriority[F]]): Functor[F] = F.value.instance
  def empty[A](implicit A: MkEmpty[A]): Empty[A] = A
  def emptyK[F[_]](implicit F: MkEmptyK[F]): EmptyK[F] = F
  def eq[T](implicit F: MkEq[T]): Eq[T] = F
  def foldable[F[_]](implicit F: MkFoldable[F]): Foldable[F] = F
  def monoid[T](implicit T: MkMonoid[T]): Monoid[T] = T
  def monoidK[F[_]](implicit F: MkMonoidK[F]): MonoidK[F] = F
  def pure[F[_]](implicit F: MkPure[F]): Pure[F] = F
  def semigroup[T](implicit F: MkSemigroup[T]): Semigroup[T] = F
  def semigroupK[F[_]](implicit F: MkSemigroupK[F]): SemigroupK[F] = F
  def show[A](implicit A: Lazy[MkShow.LowPriority[A]]): Show[A] = A.value.instance
  def iterable[F[_], A](fa: F[A])(implicit mif: MkIterable[F]): Iterable[A] = mif.iterable(fa)
}
