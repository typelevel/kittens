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

@implicitNotFound("Could not derive an instance of Applicative[${F}]")
trait MkApplicative[F[_]] extends Applicative[F]

object MkApplicative extends MkApplicativeDerivation {
  def apply[F[_]](implicit F: MkApplicative[F]): MkApplicative[F] = F
}

private[derived] abstract class MkApplicativeDerivation extends MkApplicativeNested {

  implicit val mkApplicativeHNil: MkApplicative[Const[HNil]#λ] = new MkApplicative[Const[HNil]#λ] {
    def pure[A](x: A) = HNil
    def ap[A, B](ff: HNil)(fa: HNil) = ff
    override def map[A, B](fa: HNil)(f: A => B) = fa
  }

  implicit def mkApplicativeConst[T](implicit T: Monoid[T]): MkApplicative[Const[T]#λ] =
    new MkApplicative[Const[T]#λ] {
      def pure[A](x: A) = T.empty
      def ap[A, B](ff: T)(fa: T) = T.combine(ff, fa)
      override def map[A, B](fa: T)(f: A => B) = fa
    }
}

private[derived] abstract class MkApplicativeNested extends MkApplicativeGeneric {

  implicit def mkApplicativeNested[F[_]](implicit F: Split1[F, ApplicativeOrMk, ApplicativeOrMk]): MkApplicative[F] =
    new MkApplicative[F] {

      def pure[A](x: A) =
        F.pack(F.fo.unify.pure(F.fi.unify.pure(x)))

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val fo = F.fo.unify
        F.pack(fo.ap(fo.map(F.unpack(ff))(fif => F.fi.unify.ap(fif)(_: F.I[A])))(F.unpack(fa)))
      }

      override def map[A, B](fa: F[A])(f: A => B) =
        F.pack(F.fo.unify.map(F.unpack(fa))(F.fi.unify.map(_)(f)))
    }
}

private[derived] abstract class MkApplicativeGeneric {
  protected type ApplicativeOrMk[F[_]] = Applicative[F] OrElse MkApplicative[F]

  implicit def mkApplicativeHCons[F[_]](implicit F: IsHCons1[F, ApplicativeOrMk, MkApplicative]): MkApplicative[F] =
    new MkApplicative[F] {

      def pure[A](x: A) =
        F.pack(F.fh.unify.pure(x), F.ft.pure(x))

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val (fhf, ftf) = F.unpack(ff)
        val (fha, fta) = F.unpack(fa)
        F.pack(F.fh.unify.ap(fhf)(fha), F.ft.ap(ftf)(fta))
      }

      override def map[A, B](fa: F[A])(f: A => B) = {
        val (fha, fta) = F.unpack(fa)
        F.pack(F.fh.unify.map(fha)(f), F.ft.map(fta)(f))
      }
    }

  implicit def mkApplicativeGeneric[F[_]](implicit F: Generic1[F, MkApplicative]): MkApplicative[F] =
    new MkApplicative[F] {
      def pure[A](x: A) = F.from(F.fr.pure(x))
      def ap[A, B](ff: F[A => B])(fa: F[A]) = F.from(F.fr.ap(F.to(ff))(F.to(fa)))
      override def map[A, B](fa: F[A])(f: A => B) = F.from(F.fr.map(F.to(fa))(f))
    }
}
