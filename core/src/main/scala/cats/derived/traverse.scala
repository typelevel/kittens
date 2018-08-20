package cats.derived

import cats.syntax.all._
import cats.{Applicative, Eval, Monoid, Traverse}
import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("Could not derive an instance of Traverse[${F}]")
trait MkTraverse[F[_]] extends Traverse[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

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
}

trait MkTraverseDerivation extends MkTraverse0 {
  implicit val mkTraverseId: MkTraverse[shapeless.Id] = new MkTraverse[shapeless.Id] {
    override def traverse[G[_] : Applicative, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] = f(fa)
  }
}

trait MkTraverse0 extends MkTraverse1 {
  // Induction step for products
  implicit def mkTraverseHcons[F[_]](implicit ihc: IsHCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
        val (hd, tl) = ihc.unpack(fa)

        (ihc.fh.unify.traverse(hd)(f), ihc.ft.traverse(tl)(f)).mapN(ihc.pack(_, _))
      }

    }

  // Induction step for coproducts
  implicit def mkTraverseCcons[F[_]](implicit icc: IsCCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
        val gUnpacked: G[Either[icc.H[B], icc.T[B]]] =
          icc.unpack(fa) match {
            case Left(hd) => icc.fh.unify.traverse(hd)(f).map(Left(_))
            case Right(tl) => icc.ft.traverse(tl)(f).map(Right(_))
          }

        gUnpacked.map(icc.pack)
      }

    }

}

trait MkTraverse1 extends MkTraverse2 {
  implicit def mkTraverseSplit[F[_]](implicit split: Split1[F, TraverseOrMk, TraverseOrMk]): MkTraverse[F] =
    new MkTraverse[F] {
      override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        split.fo.unify.traverse(split.unpack(fa))(split.fi.unify.traverse(_)(f)).map(split.pack)

    }
}

trait MkTraverse2 extends MkTraverse3 {
  implicit def mkTraverseGeneric[F[_]](implicit gen: Generic1[F, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {
      override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        gen.fr.traverse(gen.to(fa))(f).map(gen.from)
    }
}

trait MkTraverse3 {

  protected type TraverseOrMk[F[_]] = Traverse[F] OrElse MkTraverse[F]

  implicit def mkTraverseConstTraverse[T]: MkTraverse[Const[T]#λ] = new MkTraverse[Const[T]#λ] {
    def traverse[G[_] : Applicative, A, B](fa: T)(f: A => G[B]): G[T] = fa.pure[G]
  }
}
