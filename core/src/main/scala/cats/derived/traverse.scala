package cats.derived

import cats.syntax.all._
import cats.{Applicative, Eval, Now, Traverse}
import shapeless._

import scala.annotation.implicitNotFound

/**
  * Based on the `MkFoldable` implementation.
  */
@implicitNotFound("Could not derive an instance of Traverse[${F}]")
trait MkTraverse[F[_]] extends Traverse[F] {

  def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]]

  def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B]

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    safeTraverse(fa)(a => Now(f(a))).value

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    safeFoldLeft(fa, b) { (b, a) => Now(f(b, a)) }.value

}

object MkTraverse extends MkTraverseDerivation {
  def apply[F[_]](implicit mff: MkTraverse[F]): MkTraverse[F] = mff
}

trait MkTraverseDerivation extends MkTraverse0 {
  implicit val mkTraverseId: MkTraverse[shapeless.Id] = new MkTraverse[shapeless.Id] {
    def safeTraverse[G[_] : Applicative, A, B](fa: Id[A])(f: A => Eval[G[B]]): Eval[G[Id[B]]] = f(fa)

    def foldRight[A, B](fa: A, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa, lb)

    def safeFoldLeft[A, B](fa: A, b: B)(f: (B, A) => Eval[B]): Eval[B] = Now(f(b, fa).value)

  }

  implicit def mkTraverseConst[T]: MkTraverse[Const[T]#λ] = new MkTraverse[Const[T]#λ] {
    override def safeTraverse[G[_] : Applicative, A, B](fa: T)(f: A => Eval[G[B]]): Eval[G[T]] =
      Now(fa.pure[G])

    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

    def safeFoldLeft[A, B](fa: T, b: B)(f: (B, A) => Eval[B]): Eval[B] = Now(b)
  }
}

private[derived] trait MkTraverse0 extends MkTraverse1 {
  // Induction step for products
  implicit def mkTraverseHcons[F[_]](implicit ihc: IsHCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = {
        for {
          ht <- Now(ihc.unpack(fa))
          th <- ihc.fh.unify.safeTraverse(ht._1)(f)
          tt <- ihc.ft.safeTraverse(ht._2)(f)
        } yield (th, tt).mapN(ihc.pack(_, _))
      }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          t <- ft.foldRight(tl, lb)(f)
          h <- fh.unify.foldRight(hd, Now(t))(f)
        } yield h
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import ihc._
        val (hd, tl) = unpack(fa)
        for {
          h <- fh.unify.safeFoldLeft(hd, b)(f)
          t <- ft.safeFoldLeft(tl, h)(f)
        } yield t
      }
    }

  // Induction step for coproducts
  implicit def mkTraverseCcons[F[_]](implicit icc: IsCCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = {
        val gUnpacked: Eval[G[Either[icc.H[B], icc.T[B]]]] =
          icc.unpack(fa) match {
            case Left(hd) => apEval[G].map(icc.fh.unify.safeTraverse(hd)(f))(Left(_))
            case Right(tl) => apEval[G].map(icc.ft.safeTraverse(tl)(f))(Right(_))
          }

        apEval[G].map(gUnpacked)(icc.pack)
      }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import icc._
        unpack(fa) match {
          case Left(hd) => fh.unify.foldRight(hd, lb)(f)
          case Right(tl) => ft.foldRight(tl, lb)(f)
        }
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import icc._
        unpack(fa) match {
          case Left(hd) => fh.unify.safeFoldLeft(hd, b)(f)
          case Right(tl) => ft.safeFoldLeft(tl, b)(f)
        }
      }
    }

}

private[derived] trait MkTraverse1 extends MkTraverse2 {
  implicit def mkTraverseSplit[F[_]](implicit split: Split1[F, TraverseOrMk, TraverseOrMk]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
        split.fo.unify.safeTraverse(split.unpack(fa))(split.fi.unify.safeTraverse(_)(f)).map(_.map(split.pack))

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        import split._
        fo.unify.foldRight(unpack(fa), lb) { (fai, lbi) => fi.unify.foldRight(fai, lbi)(f) }
      }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = {
        import split._
        fo.unify.safeFoldLeft(unpack(fa), b) { (lbi, fai) => fi.unify.safeFoldLeft(fai, lbi)(f) }
      }
    }
}

private[derived] trait MkTraverse2 extends MkTraverseUtils {
  implicit def mkTraverseGeneric[F[_]](implicit gen: Generic1[F, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
        gen.fr.safeTraverse(gen.to(fa))(f).map(_.map(gen.from))

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        gen.fr.foldRight(gen.to(fa), lb)(f)

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
        gen.fr.safeFoldLeft(gen.to(fa), b)(f)
    }
}

private[derived] trait MkTraverseUtils {

  protected type TraverseOrMk[F[_]] = Traverse[F] OrElse MkTraverse[F]

  protected def apEval[G[_] : Applicative] = Applicative[Eval].compose[G]

  protected implicit class SafeTraverse[F[_]](val F: Traverse[F]) {
    def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = F match {
      case mk: MkTraverse[F] => mk.safeTraverse(fa)(f)
      case _ => F.traverse[λ[t => Eval[G[t]]], A, B](fa)(f)(apEval[G])
    }

    def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] = F match {
      case mff: MkTraverse[F] => mff.safeFoldLeft(fa, b)(f)
      case _ => Now(F.foldLeft(fa, b) { (b, a) => f(b, a).value })
    }
  }

}
