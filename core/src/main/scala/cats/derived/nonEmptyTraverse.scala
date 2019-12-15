/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.{Applicative, Apply, Eval, Foldable, NonEmptyTraverse, Reducible, Traverse}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of NonEmptyTraverse[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type λ[x => G[H[x]]] where G: NonEmptyTraverse and H: NonEmptyTraverse
  * it is a generic case class where at least one field has a NonEmptyTraverse and the rest Traverse instances
  * it is a generic sealed trait where all subclasses have a NonEmptyTraverse instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkNonEmptyTraverse[F[_]] extends NonEmptyTraverse[F] with MkTraverse[F] with MkReducible[F] {
  def safeNonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]]

  def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    safeNonEmptyTraverse(fa)(a => Eval.later(f(a))).value

  override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    super[MkTraverse].traverse(fa)(f)

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    super[MkTraverse].foldLeft(fa, b)(f)
}

object MkNonEmptyTraverse extends MkNonEmptyTraverseDerivation {
  def apply[F[_]](implicit F: MkNonEmptyTraverse[F]): MkNonEmptyTraverse[F] = F
}

private[derived] abstract class MkNonEmptyTraverseDerivation extends MkNonEmptyTraverseBase {

  implicit def mkNonEmptyTraverseNested[F[_]](
    implicit F: Split1[F, NonEmptyTraverseOrMk, NonEmptyTraverseOrMk]
  ): MkNonEmptyTraverse[F] = new MkNonEmptyTraverseInstance(
    MkTraverse.mkTraverseNested(F.asInstanceOf[Split1[F, TraverseOrMk, TraverseOrMk]]),
    MkReducible.mkReducibleNested(F.asInstanceOf[Split1[F, ReducibleOrMk, ReducibleOrMk]])
  ) {
    def safeNonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Apply[G]) =
      mkSafeNonEmptyTraverse(F.fo)(F.unpack(fa))(mkSafeNonEmptyTraverse(F.fi)(_)(f)).map(G.map(_)(F.pack))
  }
}

private[derived] abstract class MkNonEmptyTraverseBase extends MkNonEmptyTraverseCons {

  implicit def mkNonEmptyTraverseHConsBase[F[_]](
    implicit F: IsHCons1[F, NonEmptyTraverseOrMk, MkTraverse]
  ): MkNonEmptyTraverse[F] = new MkNonEmptyTraverseInstance(
    MkTraverse.mkTraverseHCons(F.asInstanceOf[IsHCons1[F, TraverseOrMk, MkTraverse]]),
    MkReducible.mkReducibleHConsBase(F.asInstanceOf[IsHCons1[F, ReducibleOrMk, MkFoldable]])
  ) {
    def safeNonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Apply[G]) =
      Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
        for {
          gfhb <- mkSafeNonEmptyTraverse(F.fh)(fha)(f)
          gftb <- F.ft.safeTraverse(fta)(f(_).map[G OrPure B](Left.apply))
        } yield gftb match {
          case Left(gftb) => G.map2(gfhb, gftb)(F.pack(_, _))
          case Right(ftb) => G.map(gfhb)(fhb => F.pack(fhb -> ftb))
        }
      }
  }

  implicit val mkNonEmptyTraverseCNil: MkNonEmptyTraverse[Const[CNil]#λ] =
    new MkNonEmptyTraverseInstance[Const[CNil]#λ](MkTraverse.mkTraverseCNil, MkReducible.mkReducibleCNil) {
      def safeNonEmptyTraverse[G[_]: Apply, A, B](fa: CNil)(f: A => Eval[G[B]]) = unexpected
    }
}

private[derived] abstract class MkNonEmptyTraverseCons extends MkNonEmptyTraverseGeneric {

  implicit def mkNonEmptyTraverseHCons[F[_]](
    implicit F: IsHCons1[F, TraverseOrMk, MkNonEmptyTraverse]
  ): MkNonEmptyTraverse[F] = new MkNonEmptyTraverseInstance(
    MkTraverse.mkTraverseHCons(F.asInstanceOf[IsHCons1[F, TraverseOrMk, MkTraverse]]),
    MkReducible.mkReducibleHCons(F.asInstanceOf[IsHCons1[F, FoldableOrMk, MkReducible]])
  ) {
    def safeNonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Apply[G]) = {
      Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
        for {
          gfhb <- MkTraverse.mkSafeTraverse(F.fh)(fha)(f(_).map[G OrPure B](Left.apply))
          gftb <- F.ft.safeNonEmptyTraverse(fta)(f)
        } yield gfhb match {
          case Left(gfhb) => G.map2(gfhb, gftb)(F.pack(_, _))
          case Right(fhb) => G.map(gftb)(ftb => F.pack(fhb -> ftb))
        }
      }
    }
  }

  implicit def mkNonEmptyTraverseCCons[F[_]](
    implicit F: IsCCons1[F, NonEmptyTraverseOrMk, MkNonEmptyTraverse]
  ): MkNonEmptyTraverse[F] = new MkNonEmptyTraverseInstance(
    MkTraverse.mkTraverseCCons(F.asInstanceOf[IsCCons1[F, TraverseOrMk, MkTraverse]]),
    MkReducible.mkReducibleCCons(F.asInstanceOf[IsCCons1[F, ReducibleOrMk, MkReducible]])
  ) {
    def safeNonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Apply[G]) =
      F.unpack(fa) match {
        case Left(fha) => mkSafeNonEmptyTraverse(F.fh)(fha)(f).map(G.map(_)(fhb => F.pack(Left(fhb))))
        case Right(fta) => F.ft.safeNonEmptyTraverse(fta)(f).map(G.map(_)(ftb => F.pack(Right(ftb))))
      }
  }
}

private[derived] abstract class MkNonEmptyTraverseGeneric {
  protected type FoldableOrMk[F[_]] = Foldable[F] OrElse MkFoldable[F]
  protected type ReducibleOrMk[F[_]] = Reducible[F] OrElse MkReducible[F]
  protected type TraverseOrMk[F[_]] = Traverse[F] OrElse MkTraverse[F]
  protected type NonEmptyTraverseOrMk[F[_]] = NonEmptyTraverse[F] OrElse MkNonEmptyTraverse[F]
  protected type OrPure[F[_], A] = Either[F[A], A]

  protected abstract class MkNonEmptyTraverseInstance[F[_]](
    mkTraverse: MkTraverse[F],
    mkReducible: MkReducible[F]
  ) extends MkNonEmptyTraverse[F] {

    def safeTraverse[G[_] : Applicative, A, B](fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] =
      mkTraverse.safeTraverse(fa)(f)

    def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]): Eval[B] =
      mkReducible.safeReduceLeftTo(fa)(f)(g)

    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      mkReducible.reduceRightTo(fa)(f)(g)

    def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
      mkReducible.safeFoldLeft(fa, b)(f)

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
      mkReducible.foldRight(fa, lb)(f)
  }

  protected def mkSafeNonEmptyTraverse[F[_], G[_]: Apply, A, B](
    F: NonEmptyTraverseOrMk[F]
  )(fa: F[A])(f: A => Eval[G[B]]): Eval[G[F[B]]] = F.unify match {
    case mk: MkNonEmptyTraverse[F] => mk.safeNonEmptyTraverse(fa)(f)
    case other => Eval.later(other.nonEmptyTraverse(fa)(f(_).value))
  }

  protected implicit def orPureApplicative[F[_]](implicit F: Apply[F]): Applicative[F OrPure *] =
    new Applicative[F OrPure *] {
      def pure[A](x: A) = Right(x)

      override def map[A, B](fa: F OrPure A)(f: A => B) = fa match {
        case Left(fa) => Left(F.map(fa)(f))
        case Right(a) => Right(f(a))
      }

      def ap[A, B](ff: F OrPure (A => B))(fa: F OrPure A) = (ff, fa) match {
        case (Left(ff), Left(fa)) => Left(F.ap(ff)(fa))
        case (Left(ff), Right(a)) => Left(F.map(ff)(_.apply(a)))
        case (Right(f), Left(fa)) => Left(F.map(fa)(f))
        case (Right(f), Right(a)) => Right(f(a))
      }
    }

  implicit def mkNonEmptyTraverseGeneric[F[_]](implicit F: Generic1[F, MkNonEmptyTraverse]): MkNonEmptyTraverse[F] =
    new MkNonEmptyTraverseInstance(
      MkTraverse.mkTraverseGeneric(F.asInstanceOf[Generic1[F, MkTraverse]]),
      MkReducible.mkReducibleGeneric(F.asInstanceOf[Generic1[F, MkReducible]])
    ) {
      def safeNonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => Eval[G[B]])(implicit G: Apply[G]) =
        F.fr.safeNonEmptyTraverse(F.to(fa))(f).map(G.map(_)(F.from))
    }
}
