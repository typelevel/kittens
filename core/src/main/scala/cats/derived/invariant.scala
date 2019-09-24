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

package cats
package derived

import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""
Could not derive an instance of Invariant[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T]
  * it is a nested type λ[x => G[H[x]]] where G: Invariant and H: Invariant
  * it is a generic case class where all fields have an Invariant instance
  * it is a generic sealed trait where all subclasses have an Invariant instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector
""".trim)
trait MkInvariant[F[_]] extends Invariant[F] {
  def safeImap[A, B](fa: F[A])(g: A => Eval[B])(f: B => Eval[A]): Eval[F[B]]
  def imap[A, B](fa: F[A])(g: A => B)(f: B => A): F[B] =
    safeImap(fa)(a => Eval.later(g(a)))(b => Eval.later(f(b))).value
}
object MkInvariant extends MkInvariantDerivation {
  def apply[F[_]](implicit F: MkInvariant[F]): MkInvariant[F] = F
}
private[derived] abstract class MkInvariantDerivation extends MkInvariantNested {
  implicit val mkInvariantHNil: MkInvariant[Const[HNil]#λ] = mkInvariantConst
  implicit val mkInvariantCNil: MkInvariant[Const[CNil]#λ] = mkInvariantConst

  implicit def mkInvariantConst[T]: MkInvariant[Const[T]#λ] =
    new MkInvariant[Const[T]#λ] {
      def safeImap[A, B](t: T)(g: A => Eval[B])(f: B => Eval[A]): Eval[T] = Eval.now(t)
    }
}

private[derived] abstract class MkInvariantNested extends MkInvariantCons {
  implicit def mkFunctorInvariantNested[F[_]](implicit F: Split1[F, InvariantOrMk, InvariantOrMk]): MkInvariant[F] =
    new MkInvariant[F] {
      def safeImap[A, B](fa: F[A])(g: A => Eval[B])(f: B => Eval[A]): Eval[F[B]] =
        mkImapSafe[F.O, F.I[B], F.I[A]](F.fo)(F.unpack(fa))(
          mkImapSafe(F.fi)(_)(g)(f))(
          mkImapSafe(F.fi)(_)(f)(g)
        ).map(F.pack)
    }
}

private[derived] abstract class MkInvariantCons extends MkInvariantGeneric {
  implicit def mkInvariantHCons[F[_]](implicit F: IsHCons1[F, InvariantOrMk, MkInvariant]): MkInvariant[F] =
    new MkInvariant[F] {
      def safeImap[A, B](fa: F[A])(g: A => Eval[B])(f: B => Eval[A]): Eval[F[B]] =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          for {
            fhb <- mkImapSafe(F.fh)(fha)(g)(f)
            ftb <- F.ft.safeImap(fta)(g)(f)
          } yield F.pack(fhb, ftb)
        }
    }

  implicit def mkInvariantCCons[F[_]](implicit F: IsCCons1[F, InvariantOrMk, MkInvariant]): MkInvariant[F] =
    new MkInvariant[F] {
      def safeImap[A, B](fa: F[A])(g: A => Eval[B])(f: B => Eval[A]): Eval[F[B]] = F.unpack(fa) match {
        case Left(fha) => mkImapSafe(F.fh)(fha)(g)(f).map(fhb => F.pack(Left(fhb)))
        case Right(fta) => F.ft.safeImap(fta)(g)(f).map(ftb => F.pack(Right(ftb)))
      }
    }
}
private[derived] abstract class MkInvariantGeneric {
  protected type InvariantOrMk[F[_]] = Invariant[F] OrElse MkInvariant[F]

  protected def mkImapSafe[F[_], A, B](F: InvariantOrMk[F])(fa: F[B])(g: B => Eval[A])(f: A => Eval[B]): Eval[F[A]] =
    F.unify match {
      case mk: MkInvariant[F] => mk.safeImap(fa)(g)(f)
      case p => Eval.later(p.imap(fa)(g(_).value)(f(_).value))
    }

  implicit def mkInvariantGeneric[F[_]](implicit F: Generic1[F, MkInvariant]): MkInvariant[F] =
    new MkInvariant[F] {
      def safeImap[A, B](fa: F[A])(g: A => Eval[B])(f: B => Eval[A]): Eval[F[B]] =
        F.fr.safeImap(F.to(fa))(g)(f).map(F.from[B])
    }
}

