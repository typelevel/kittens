package cats.derived

import cats.derived.MkTraverse.SafeTraverse
import cats.syntax.all._
import cats.{Applicative, Eval, Monoid, Traverse}
import shapeless._

import scala.annotation.implicitNotFound

/**
  * This trait extends `Traverse` and implements it's fold methods.
  * The overriden implementation can be removed as soon as issue
  * <a href="https://github.com/typelevel/cats/issues/107#issuecomment-393797529">cats-107</a>
  * gets resolved.
  * The current implementation is based on a proposal posted by @Baccata in a comment to the issue.
  *
  */
@implicitNotFound("Could not derive an instance of Traverse[${F}]")
trait MkTraverse[F[_]] extends Traverse[F] {

  override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[F[B]] =
    safeTraverse(fa)(a => Eval.now(f(a))).value

  def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]]

  override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = {
    traverse[cats.data.Const[B, ?], A, B](fa) { a => cats.data.Const(f(a)) } getConst
  }

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    foldMap(fa)(f.curried andThen defer)(fMonoid(_ compose _)).apply(lb)
  }

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldMap[A, B => B](fa) { a => b => f(b, a) }(fMonoid(_ andThen _)).apply(b)

  private def fMonoid[A](c: (A => A, A => A) => (A => A)): Monoid[A => A] = new Monoid[A => A] {
    def combine(f: A => A, g: A => A): A => A = c(f, g)

    def empty: A => A = identity
  }

  private def defer[B](f: Eval[B] => Eval[B]): Eval[B] => Eval[B] =
    evalB => Eval.defer(f(evalB))

}

object MkTraverse extends MkTraverseDerivation {
  def apply[F[_]](implicit mff: MkTraverse[F]): MkTraverse[F] = mff

  private[derived] implicit class SafeTraverse[F[_]](val F: Traverse[F]) extends AnyVal {
    def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = F match {
      case mk: MkTraverse[F] => mk.safeTraverse(fa)(f)
      case _ => F.traverse[λ[t => Eval[G[t]]], A, B](fa)(f)(apEval[G])
    }
  }
}

trait MkTraverseDerivation extends MkTraverse0 {
  implicit val mkTraverseId: MkTraverse[shapeless.Id] = new MkTraverse[shapeless.Id] {
    override def safeTraverse[G[_] : Applicative, A, B](fa: Id[A])(f: A => Eval[G[B]]): Eval[G[Id[B]]] = f(fa)
  }

  implicit def mkTraverseConst[T]: MkTraverse[Const[T]#λ] = new MkTraverse[Const[T]#λ] {
    override def safeTraverse[G[_] : Applicative, A, B](fa: T)(f: A => Eval[G[B]]): Eval[G[T]] =
      Eval.now(fa.pure[G])
  }
}

private[derived] trait MkTraverse0 extends MkTraverse1 {
  // Induction step for products
  implicit def mkTraverseHcons[F[_]](implicit ihc: IsHCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = {
        for {
          ht <- Eval.now(ihc.unpack(fa))
          th <- ihc.fh.unify.safeTraverse(ht._1)(f)
          tt <- ihc.ft.safeTraverse(ht._2)(f)
        } yield (th, tt).mapN(ihc.pack(_, _))
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
    }

}

private[derived] trait MkTraverse1 extends MkTraverse2 {
  implicit def mkTraverseSplit[F[_]](implicit split: Split1[F, TraverseOrMk, TraverseOrMk]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
        split.fo.unify.safeTraverse(split.unpack(fa))(split.fi.unify.safeTraverse(_)(f)).map(_.map(split.pack))
    }
}

private[derived] trait MkTraverse2 extends MkTraverseUtils {
  implicit def mkTraverseGeneric[F[_]](implicit gen: Generic1[F, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
        gen.fr.safeTraverse(gen.to(fa))(f).map(_.map(gen.from))
    }
}

private[derived] trait MkTraverseUtils {

  protected type TraverseOrMk[F[_]] = Traverse[F] OrElse MkTraverse[F]

  protected def apEval[G[_] : Applicative] = Applicative[Eval].compose[G]

}
