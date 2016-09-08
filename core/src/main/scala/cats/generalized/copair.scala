package cats.generalized

import cats.{Applicative, Bitraverse, Eval}
import cats.data.{Validated, Xor}
import shapeless._

trait Copair[F[_,_]] extends Bitraverse[F] {
  def left[A,B](a: A): F[A,B]
  def right[A,B](b: B): F[A,B]

  def fold[A,B,C](f: F[A,B])(fa: A => C, fb: B => C): C

  def isRight[A,B](f: F[A,B]): Boolean = fold(f)(_ => false, _ => true )
  def isLeft[A,B](f: F[A,B]):  Boolean = fold(f)(_ => true , _ => false)

  def foreach[A,B](f: F[A,B])(fn: B => Unit): Unit = fold(f)(_ => (), fn)

  def swap[A,B](f: F[A,B]): F[B,A] = fold(f)(right[B, A], left[B, A])

  def swapped[A,B,C,D](f: F[A,B])(fn: F[B,A] => F[C,D]): F[D,C] = swap(fn(swap(f)))

  def forall[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => true, fn)

  def exists[A, B](f: F[A,B])(fn: B => Boolean): Boolean = fold(f)(_ => false, fn)

  def to[G[_, _], A, B](f: F[A, B])(implicit G: Copair[G]): G[A,B] = fold(f)(G.left, G.right)

  def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[F[C, D]] =
    fold(fab)(
      l => G.map(f(l))(left),
      r => G.map(g(r))(right)
    )

  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    fold(fab)(
      l => f(c,l),
      r => g(c,r)
    )

  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(fab)(
      l => f(l,c),
      r => g(r,c)
    )
}

trait CopairSyntax {
  implicit def copairSyntax[F[_, _]: Copair, A, B](fab: F[A, B]): CopairOps[F, A, B] =
    new CopairOps[F, A, B](fab)

  implicit def copairIdSyntax[A](a: A): CopairIdOps[A] = new CopairIdOps[A](a)
}

trait CopairInstances {

  implicit def validatedCopair: Copair[Validated] =
    new Copair[Validated] {
      def fold[A, B, C](f: Validated[A, B])(fa: (A) => C, fb: (B) => C): C =
        f match {
          case Validated.Invalid(a) => fa(a)
          case Validated.Valid(b) => fb(b)
        }

      def left[A, B](a: A): Validated[A, B] = Validated.Invalid(a)
      def right[A, B](b: B): Validated[A, B] = Validated.Valid(b)
    }

  implicit def xorCopair: Copair[Xor] =
    new Copair[Xor] {
      def fold[A, B, C](f: Xor[A, B])(fa: (A) => C, fb: (B) => C): C =
        f match {
          case Xor.Left(a) => fa(a)
          case Xor.Right(b) => fb(b)
        }

      def left[A, B](a: A): Xor[A, B] = Xor.Left(a)
      def right[A, B](b: B): Xor[A, B] = Xor.Right(b)
    }

  implicit val eitherCopair: Copair[Either] =
    new Copair[Either] {
      def fold[A, B, C](f: Either[A, B])(fa: (A) => C, fb: (B) => C): C =
        f match {
          case Left(a) => fa(a)
          case Right(b) => fb(b)
        }

      def left[A, B](a: A): Either[A, B] = Left(a)
      def right[A, B](b: B): Either[A, B] = Right(b)
    }

  type ShapelessCoproduct2[A, B] = A :+: B :+: CNil
  implicit val shapelessCoproductCopair: Copair[ShapelessCoproduct2] =
    new Copair[ShapelessCoproduct2] {
      def fold[A, B, C](f: CopairInstances.this.ShapelessCoproduct2[A,B])(fa: A => C,fb: B => C): C =
        f match {
          case Inl(a) => fa(a)
          case Inr(Inl(b)) => fb(b)
          case Inr(Inr(_)) => sys.error("impossible")
        }

      def left[A, B](a: A): CopairInstances.this.ShapelessCoproduct2[A,B] = Coproduct[ShapelessCoproduct2[A,B]](a)
      def right[A, B](b: B): CopairInstances.this.ShapelessCoproduct2[A,B] = Coproduct[ShapelessCoproduct2[A,B]](b)
    }
}

final class CopairOps[F[_,_], A, B](pair: F[A,B])(implicit F: Copair[F]) {

  def fold[C](fa: A => C, fb: B => C): C = F.fold(pair)(fa, fb)
  def swap: F[B,A] = F.swap(pair)

  def isRight: Boolean = F.isRight(pair)
  def isLeft:  Boolean = F.isLeft(pair)

  def foreach(fn: B => Unit): Unit = F.foreach(pair)(fn)

  def forall(fn: B => Boolean): Boolean = F.forall(pair)(fn)

  def exists(fn: B => Boolean): Boolean = F.exists(pair)(fn)

  def to[G[_, _]](implicit G: Copair[G]): G[A,B] = F.to[G,A,B](pair)

}

final class CopairIdOps[A](a: A) {
  def leftC[F[_,_], B](implicit F: Copair[F]): F[A,B] = F.left(a)
  def rightC[F[_,_], B](implicit F: Copair[F]): F[B,A] = F.right(a)
}
