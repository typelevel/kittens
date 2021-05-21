package cats.derived

import cats.*

private[derived] trait Instances extends Instances1:

  given [X](using X: Monoid[X]): MonoidK[Const[X]] with
    def empty[A]: Const[X][A] = X.empty

    def combineK[A](x: Const[X][A], y: Const[X][A]): Const[X][A] =
      X.combine(x, y)

  given [X]: Traverse[Const[X]] with Contravariant[Const[X]] with
    override def map[A, B](fa: Const[X][A])(f: A => B): Const[X][B] = fa

    def contramap[A, B](fa: X)(f: B => A): X = fa

    def foldLeft[A, B](fa: Const[X][A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[X][A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

    def traverse[G[_]: Applicative, A, B](fa: Const[X][A])(f: A => G[B]): G[Const[X][B]] =
      Applicative[G].pure(fa)

private[derived] trait Instances1:
  given [X](using X: Semigroup[X]): SemigroupK[Const[X]] with
    def combineK[A](x: Const[X][A], y: Const[X][A]): Const[X][A] =
      X.combine(x, y)
