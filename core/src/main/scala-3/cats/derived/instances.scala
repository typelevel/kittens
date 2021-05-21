package cats.derived

import cats.{Contravariant, Functor, Monoid, MonoidK, Semigroup, SemigroupK}

private[derived] trait Instances extends Instances1:

  given [X]: Functor[Const[X]] with
    def map[A, B](fa: X)(f: A => B): X = fa

  given [X](using X: Monoid[X]): MonoidK[Const[X]] with
    def empty[A]: Const[X][A] = X.empty

    def combineK[A](x: Const[X][A], y: Const[X][A]): Const[X][A] =
      X.combine(x, y)

private[derived] trait Instances1:
  given [X](using X: Semigroup[X]): SemigroupK[Const[X]] with
    def combineK[A](x: Const[X][A], y: Const[X][A]): Const[X][A] =
      X.combine(x, y)

  given [X]: Contravariant[Const[X]] with
    def contramap[A, B](fa: X)(f: B => A): X = fa
