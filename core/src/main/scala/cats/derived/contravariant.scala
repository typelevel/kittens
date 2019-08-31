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

@implicitNotFound("Could not derive an instance of Contravariant[${F}]")
trait MkContravariant[F[_]] extends Contravariant[F] {
  def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]]
  def contramap[A, B](fa: F[A])(f: B => A): F[B] = safeContramap(fa)((b: B) => Eval.later(f(b))).value
}

object MkContravariant extends MkContravariantDerivation {
  def apply[F[_]](implicit F: MkContravariant[F]): MkContravariant[F] = F
}

private[derived] abstract class MkContravariantDerivation extends MkFunctorContraNested {
  implicit val mkContraHNil: MkContravariant[Const[HNil]#λ] = mkContraConst
  implicit val mkContraCNil: MkContravariant[Const[CNil]#λ] = mkContraConst

  implicit def mkContraConst[T]: MkContravariant[Const[T]#λ] =
    new MkContravariant[Const[T]#λ] {
      def safeContramap[A, B](t: T)(f: B => Eval[A]): Eval[T] = Eval.now(t)
    }
}

private[derived] abstract class MkFunctorContraNested extends MkContravariantCons {

  implicit def mkFunctorContraNested[F[_]](implicit F: Split1[F, Functor, ContraOrMk]): MkContravariant[F] =
    new MkContravariant[F] {

      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        Eval.later(F.pack(F.fo.map(F.unpack(fa))(mkContraSafe(F.fi)(_)(f).value)))
    }
}

private[derived] abstract class MkContravariantCons extends MkContravariantGeneric {

  implicit def mkContraHCons[F[_]](implicit F: IsHCons1[F, ContraOrMk, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {

      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          for {
            fhb <- mkContraSafe(F.fh)(fha)(f)
            ftb <- F.ft.safeContramap(fta)(f)
          } yield F.pack(fhb, ftb)
        }
    }

  implicit def mkContraCCons[F[_]](implicit F: IsCCons1[F, ContraOrMk, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {

      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] = F.unpack(fa) match {
        case Left(fha) => mkContraSafe(F.fh)(fha)(f).map(fhb => F.pack(Left(fhb)))
        case Right(fta) => F.ft.safeContramap(fta)(f).map(ftb => F.pack(Right(ftb)))
      }
    }
}

private[derived] abstract class MkContravariantGeneric {
  protected type ContraOrMk[F[_]] = Contravariant[F] OrElse MkContravariant[F]

  protected def mkContraSafe[F[_], A, B](F: ContraOrMk[F])(fa: F[B])(f: A => Eval[B]): Eval[F[A]] =
    F.unify match {
      case mk: MkContravariant[F] => mk.safeContramap(fa)(f)
      case p => Eval.later(p.contramap(fa)(f(_).value))
    }

  implicit def mkContraGeneric[F[_]](implicit F: Generic1[F, MkContravariant]): MkContravariant[F] =
    new MkContravariant[F] {

      def safeContramap[A, B](fa: F[A])(f: B => Eval[A]): Eval[F[B]] =
        F.fr.safeContramap(F.to(fa))(f).map(F.from)
    }
}

