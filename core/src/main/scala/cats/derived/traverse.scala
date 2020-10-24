package cats.derived

import cats.{Applicative, Eval, Traverse}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of Traverse[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T]
  * it is a nested type λ[x => G[H[x]]] where G: Traverse and H: Traverse
  * it is a generic case class where all fields have a Traverse instance
  * it is a generic sealed trait where all subclasses have a Traverse instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkTraverse[F[_]] extends Traverse[F] with MkFoldable[F] {
  def safeTraverse[G[_]: Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]]
  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    safeTraverse(fa)(a => Eval.later(f(a))).value

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B) =
    super[MkFoldable].foldLeft(fa, b)(f)
}

object MkTraverse extends MkTraverseDerivation {
  def apply[F[_]](implicit F: MkTraverse[F]): MkTraverse[F] = F
}

abstract private[derived] class MkTraverseDerivation extends MkTraverseNested {
  implicit val mkTraverseHNil: MkTraverse[Const[HNil]#λ] = mkTraverseConst
  implicit val mkTraverseCNil: MkTraverse[Const[CNil]#λ] = mkTraverseConst

  implicit def mkTraverseConst[T]: MkTraverse[Const[T]#λ] = new MkTraverse[Const[T]#λ] {
    def safeTraverse[G[_], A, B](fa: T)(f: A => Eval[G[B]])(implicit G: Applicative[G]) = Eval.now(G.pure(fa))
    def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = lb
    def safeFoldLeft[A, B](fa: T, b: B)(f: (B, A) => Eval[B]) = Eval.now(b)
  }
}

abstract private[derived] class MkTraverseNested extends MkTraverseCons {

  implicit def mkTraverseNested[F[_]](implicit F: Split1[F, TraverseOrMk, TraverseOrMk]): MkTraverse[F] =
    new MkTraverse[F] {

      def safeTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Applicative[G]) =
        mkSafeTraverse(F.fo)(F.unpack(fa))(mkSafeTraverse(F.fi)(_)(f)).map(G.map(_)(F.pack))

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        F.fo.unify.foldRight(F.unpack(fa), lb)((fia, lb) => F.fi.unify.foldRight(fia, lb)(f))

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        mkSafeFoldLeft(F.fo)(F.unpack(fa), b)((b, fia) => mkSafeFoldLeft(F.fi)(fia, b)(f))
    }
}

abstract private[derived] class MkTraverseCons extends MkTraverseGeneric {

  implicit def mkTraverseHCons[F[_]](implicit F: IsHCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {

      def safeTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Applicative[G]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          for {
            gfhb <- mkSafeTraverse(F.fh)(fha)(f)
            gftb <- F.ft.safeTraverse(fta)(f)
          } yield G.map2(gfhb, gftb)(F.pack(_, _))
        }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          F.fh.unify.foldRight(fha, F.ft.foldRight(fta, lb)(f))(f)
        }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          mkSafeFoldLeft(F.fh)(fha, b)(f).flatMap(F.ft.safeFoldLeft(fta, _)(f))
        }
    }

  implicit def mkTraverseCCons[F[_]](implicit F: IsCCons1[F, TraverseOrMk, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {

      def safeTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Applicative[G]) =
        F.unpack(fa) match {
          case Left(fha) => mkSafeTraverse(F.fh)(fha)(f).map(G.map(_)(fhb => F.pack(Left(fhb))))
          case Right(fta) => F.ft.safeTraverse(fta)(f).map(G.map(_)(ftb => F.pack(Right(ftb))))
        }

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        F.unpack(fa) match {
          case Left(fha) => F.fh.unify.foldRight(fha, lb)(f)
          case Right(fta) => F.ft.foldRight(fta, lb)(f)
        }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        F.unpack(fa) match {
          case Left(fha) => mkSafeFoldLeft(F.fh)(fha, b)(f)
          case Right(fta) => F.ft.safeFoldLeft(fta, b)(f)
        }
    }
}

abstract private[derived] class MkTraverseGeneric {
  protected type TraverseOrMk[F[_]] = Traverse[F] OrElse MkTraverse[F]

  private[derived] def mkSafeTraverse[F[_], G[_]: Applicative, A, B](
      F: TraverseOrMk[F]
  )(fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = F.unify match {
    case mk: MkTraverse[F] => mk.safeTraverse(fa)(f)
    case other => other.traverse[λ[t => Eval[G[t]]], A, B](fa)(f)(Applicative[Eval].compose[G])
  }

  protected def mkSafeFoldLeft[F[_], A, B](F: TraverseOrMk[F])(fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
    F.unify match {
      case mk: MkTraverse[F] => mk.safeFoldLeft(fa, b)(f)
      case other => Eval.later(other.foldLeft(fa, b)(f(_, _).value))
    }

  implicit def mkTraverseGeneric[F[_]](implicit F: Generic1[F, MkTraverse]): MkTraverse[F] =
    new MkTraverse[F] {

      def safeTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Applicative[G]) =
        F.fr.safeTraverse(F.to(fa))(f).map(G.map(_)(F.from))

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        F.fr.foldRight(F.to(fa), lb)(f)

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        F.fr.safeFoldLeft(F.to(fa), b)(f)
    }
}
