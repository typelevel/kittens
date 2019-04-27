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

import scala.annotation.implicitNotFound

@implicitNotFound("Could not derive an instance of Functor[${F}]")
trait MkFunctor[F[_]] extends Functor[F] {
  def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    safeMap(fa)(a => Eval.later(f(a))).value
}

object MkFunctor extends MkFunctorDerivation {
  def apply[F[_]](implicit F: MkFunctor[F]): MkFunctor[F] = F
}

private[derived] abstract class MkFunctorDerivation extends MkFunctorNested {
  implicit val mkFunctorHNil: MkFunctor[Const[HNil]#λ] = mkFunctorConst
  implicit val mkFunctorCNil: MkFunctor[Const[CNil]#λ] = mkFunctorConst

  implicit def mkFunctorConst[T]: MkFunctor[Const[T]#λ] =
    new MkFunctor[Const[T]#λ] {
      def safeMap[A, B](t: T)(f: A => Eval[B]) = Eval.now(t)
    }
}

private[derived] abstract class MkFunctorNested extends MkFunctorGeneric {

  implicit def mkFunctorNested[F[_]](implicit F: Split1[F, FunctorOrMk, FunctorOrMk]): MkFunctor[F] =
    new MkFunctor[F] {

      def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
        mkSafeMap(F.fo)(F.unpack(fa))(mkSafeMap(F.fi)(_)(f)).map(F.pack)
    }
}

private[derived] abstract class MkFunctorGeneric {
  protected type FunctorOrMk[F[_]] = Functor[F] OrElse MkFunctor[F]

  protected def mkSafeMap[F[_], A, B](F: FunctorOrMk[F])(fa: F[A])(f: A => Eval[B]): Eval[F[B]] =
    F.unify match {
      case mk: MkFunctor[F] => mk.safeMap(fa)(f)
      case p => Eval.later(p.map(fa)(f(_).value))
    }

  implicit def mkFunctorHCons[F[_]](implicit F: IsHCons1[F, FunctorOrMk, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {

      def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
        Eval.now(F.unpack(fa)).flatMap { case (fha, fta) =>
          for {
            fhb <- mkSafeMap(F.fh)(fha)(f)
            ftb <- F.ft.safeMap(fta)(f)
          } yield F.pack(fhb, ftb)
        }
    }

  implicit def mkFunctorCCons[F[_]](implicit F: IsCCons1[F, FunctorOrMk, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {

      def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = F.unpack(fa) match {
        case Left(fha) => mkSafeMap(F.fh)(fha)(f).map(fhb => F.pack(Left(fhb)))
        case Right(fta) => F.ft.safeMap(fta)(f).map(ftb => F.pack(Right(ftb)))
      }
    }

  implicit def mkFunctorGeneric[F[_]](implicit F: Generic1[F, MkFunctor]): MkFunctor[F] =
    new MkFunctor[F] {

      def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
        F.fr.safeMap(F.to(fa))(f).map(F.from)
    }
}
