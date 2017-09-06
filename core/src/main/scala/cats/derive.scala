package cats
import alleycats.{EmptyK, Pure}
import cats.derived._

object derive {
  def functor[F[_]](implicit F: MkFunctor[F]) : Functor[F] = F
  def emptyK[F[_]](implicit F: MkEmptyK[F]): EmptyK[F] = F
  def eq[T](implicit F: MkEq[T]): Eq[T] = F
  def foldable[F[_]](implicit F: MkFoldable[F]): Foldable[F] = F
  def monoid[T](implicit T: MkMonoid[T]): Monoid[T] = T
  def monoidK[F[_]](implicit F: MkMonoidK[F]): MonoidK[F] = F
  def pure[F[_]](implicit F: MkPure[F]): Pure[F] = F
  def semigroup[T](implicit F: MkSemigroup[T]): Semigroup[T] = F
  def semigroupK[F[_]](implicit F: MkSemigroupK[F]): SemigroupK[F] = F
  def show[T](implicit F: MkShow[T]): Show[T] = F

  def iterable[F[_], A](fa: F[A])(implicit mif: MkIterable[F]): Iterable[A] = mif.iterable(fa)

}
