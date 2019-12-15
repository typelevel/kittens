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

import cats.{Eval, Foldable, Reducible}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of Reducible[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type λ[x => G[H[x]]] where G: Reducible and H: Reducible
  * it is a generic case class where at least one field has a Reducible and the rest Foldable instances
  * it is a generic sealed trait where all subclasses have a Reducible instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkReducible[F[_]] extends Reducible[F] with MkFoldable[F] {
  def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]): Eval[B]

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
    safeReduceLeftTo(fa)(f)((b, a) => Eval.later(g(b, a))).value
}

object MkReducible extends MkReducibleDerivation {
  def apply[F[_]](implicit F: MkReducible[F]): MkReducible[F] = F
}

private[derived] abstract class MkReducibleDerivation extends MkReducibleBase {

  implicit def mkReducibleNested[F[_]](implicit F: Split1[F, ReducibleOrMk, ReducibleOrMk]): MkReducible[F] =
    new MkReducibleInstance(MkFoldable.mkFoldableNested(F.asInstanceOf[Split1[F, FoldableOrMk, FoldableOrMk]])) {

      def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]) =
        mkSafeReduceLeftTo(F.fo)(F.unpack(fa))(mkSafeReduceLeftTo(F.fi)(_)(f)(g)) {
          (lb, fia) => lb.map(MkFoldable.mkSafeFoldLeft(F.fi)(fia, _)(g))
        }.flatMap(identity)

      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) = {
        val fo = F.fo.unify
        val fi = F.fi.unify
        fo.reduceRightTo(F.unpack(fa))(fi.reduceRightTo(_)(f)(g)) {
          (fia, llb) => Eval.later(fi.foldRight(fia, llb.value)(g))
        }.flatMap(identity)
      }
    }
}

private[derived] abstract class MkReducibleBase extends MkReducibleCons {

  implicit def mkReducibleHConsBase[F[_]](implicit F: IsHCons1[F, ReducibleOrMk, MkFoldable]): MkReducible[F] =
    new MkReducibleInstance(MkFoldable.mkFoldableHCons(F.asInstanceOf[IsHCons1[F, FoldableOrMk, MkFoldable]])) {

      def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          mkSafeReduceLeftTo(F.fh)(fha)(f)(g).flatMap(F.ft.safeFoldLeft(fta, _)(g))
        }

      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          F.ft.reduceRightToOption(fta)(f)(g).flatMap {
            case Some(b) => F.fh.unify.foldRight(fha, Eval.now(b))(g)
            case None => F.fh.unify.reduceRightTo(fha)(f)(g)
          }
        }
    }

  implicit val mkReducibleCNil: MkReducible[Const[CNil]#λ] =
    new MkReducibleInstance[Const[CNil]#λ](MkFoldable.mkFoldableCNil) {
      def safeReduceLeftTo[A, B](fa: CNil)(f: A => B)(g: (B, A) => Eval[B]) = unexpected
      def reduceRightTo[A, B](fa: CNil)(f: A => B)(g: (A, Eval[B]) => Eval[B]) = unexpected
    }
}

private[derived] abstract class MkReducibleCons extends MkReducibleGeneric {

  implicit def mkReducibleHCons[F[_]](implicit F: IsHCons1[F, FoldableOrMk, MkReducible]): MkReducible[F] =
    new MkReducibleInstance(MkFoldable.mkFoldableHCons(F.asInstanceOf[IsHCons1[F, FoldableOrMk, MkFoldable]])) {

      def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          MkFoldable.mkSafeFoldLeft(F.fh)(fha, Option.empty[B]) {
            case (Some(b), a) => g(b, a).map(Some.apply)
            case (None, a) => Eval.now(Some(f(a)))
          }.flatMap {
            case Some(b) => F.ft.safeFoldLeft(fta, b)(g)
            case None => F.ft.safeReduceLeftTo(fta)(f)(g)
          }
        }

      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          F.fh.unify.foldRight(fha, F.ft.reduceRightTo(fta)(f)(g))(g)
        }
    }

  implicit def mkReducibleCCons[F[_]](implicit F: IsCCons1[F, ReducibleOrMk, MkReducible]): MkReducible[F] =
    new MkReducibleInstance(MkFoldable.mkFoldableCCons(F.asInstanceOf[IsCCons1[F, FoldableOrMk, MkFoldable]])) {

      def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]) =
        F.unpack(fa) match {
          case Left(fha) => mkSafeReduceLeftTo(F.fh)(fha)(f)(g)
          case Right(fta) => F.ft.safeReduceLeftTo(fta)(f)(g)
        }

      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        F.unpack(fa) match {
          case Left(fha) => F.fh.unify.reduceRightTo(fha)(f)(g)
          case Right(fta) => F.ft.reduceRightTo(fta)(f)(g)
        }
    }
}

private[derived] abstract class MkReducibleGeneric {
  protected type FoldableOrMk[F[_]] = Foldable[F] OrElse MkFoldable[F]
  protected type ReducibleOrMk[F[_]] = Reducible[F] OrElse MkReducible[F]

  protected abstract class MkReducibleInstance[F[_]](foldable: MkFoldable[F]) extends MkReducible[F] {

    def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
      foldable.safeFoldLeft(fa, b)(f)

    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
      foldable.foldRight(fa, lb)(f)
  }

  protected def mkSafeReduceLeftTo[F[_], A, B](
    F: ReducibleOrMk[F]
  )(fa: F[A])(f: A => B)(g: (B, A) => Eval[B]): Eval[B] = F.unify match {
    case mk: MkReducible[F] => mk.safeReduceLeftTo(fa)(f)(g)
    case other => Eval.later(other.reduceLeftTo(fa)(f)(g(_, _).value))
  }

  implicit def mkReducibleGeneric[F[_]](implicit F: Generic1[F, MkReducible]): MkReducible[F] =
    new MkReducibleInstance(MkFoldable.mkFoldableGeneric(F.asInstanceOf[Generic1[F, MkFoldable]])) {

      def safeReduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => Eval[B]) =
        F.fr.safeReduceLeftTo(F.to(fa))(f)(g)

      def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        F.fr.reduceRightTo(F.to(fa))(f)(g)
    }
}
