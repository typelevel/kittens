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

import cats.{Eval, Foldable}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of Foldable[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T]
  * it is a nested type λ[x => G[H[x]]] where G: Foldable and H: Foldable
  * it is a generic case class where all fields have a Foldable instance
  * it is a generic sealed trait where all subclasses have a Foldable instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkFoldable[F[_]] extends Foldable[F] {
  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B]

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    safeFoldLeft(fa, b)((b, a) => Eval.later(f(b, a))).value
}

object MkFoldable extends MkFoldableDerivation {
  def apply[F[_]](implicit F: MkFoldable[F]): MkFoldable[F] = F
}

abstract private[derived] class MkFoldableDerivation extends MkFoldableNested {
  implicit val mkFoldableHNil: MkFoldable[Const[HNil]#λ] = mkFoldableConst
  implicit val mkFoldableCNil: MkFoldable[Const[CNil]#λ] = mkFoldableConst

  implicit def mkFoldableConst[T]: MkFoldable[Const[T]#λ] =
    new MkFoldable[Const[T]#λ] {
      def foldRight[A, B](fa: T, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = lb
      def safeFoldLeft[A, B](fa: T, b: B)(f: (B, A) => Eval[B]) = Eval.now(b)
    }
}

abstract private[derived] class MkFoldableNested extends MkFoldableCons {

  implicit def mkFoldableNested[F[_]](implicit F: Split1[F, FoldableOrMk, FoldableOrMk]): MkFoldable[F] =
    new MkFoldable[F] {

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        F.fo.unify.foldRight(F.unpack(fa), lb)(F.fi.unify.foldRight(_, _)(f))

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        mkSafeFoldLeft(F.fo)(F.unpack(fa), b)((b, fia) => mkSafeFoldLeft(F.fi)(fia, b)(f))
    }
}

abstract private[derived] class MkFoldableCons extends MkFoldableGeneric {

  implicit def mkFoldableHCons[F[_]](implicit F: IsHCons1[F, FoldableOrMk, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fhb) =>
          F.fh.unify.foldRight(fha, F.ft.foldRight(fhb, lb)(f))(f)
        }

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fhb) =>
          mkSafeFoldLeft(F.fh)(fha, b)(f).flatMap(F.ft.safeFoldLeft(fhb, _)(f))
        }
    }

  implicit def mkFoldableCCons[F[_]](implicit F: IsCCons1[F, FoldableOrMk, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {

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

abstract private[derived] class MkFoldableGeneric {
  protected type FoldableOrMk[F[_]] = Foldable[F] OrElse MkFoldable[F]

  private[derived] def mkSafeFoldLeft[F[_], A, B](F: FoldableOrMk[F])(fa: F[A], b: B)(f: (B, A) => Eval[B]): Eval[B] =
    F.unify match {
      case mk: MkFoldable[F] => mk.safeFoldLeft(fa, b)(f)
      case other => Eval.later(other.foldLeft(fa, b)(f(_, _).value))
    }

  implicit def mkFoldableGeneric[F[_]](implicit F: Generic1[F, MkFoldable]): MkFoldable[F] =
    new MkFoldable[F] {

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        F.fr.foldRight(F.to(fa), lb)(f)

      def safeFoldLeft[A, B](fa: F[A], b: B)(f: (B, A) => Eval[B]) =
        F.fr.safeFoldLeft(F.to(fa), b)(f)
    }
}
