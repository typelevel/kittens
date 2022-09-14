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

@implicitNotFound("""Could not derive an instance of Apply[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Semigroup
  * it is a nested type λ[x => G[H[x]]] where G: Apply and H: Apply
  * it is a generic case class where all fields have an Apply instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkApply[F[_]] extends Apply[F]

object MkApply extends MkApplyDerivation {
  def apply[F[_]](implicit F: MkApply[F]): MkApply[F] = F
}

abstract private[derived] class MkApplyDerivation extends MkApplyNested {

  implicit val mkApplyHNil: MkApply[Const[HNil]#λ] = new MkApply[Const[HNil]#λ] {
    def ap[A, B](ff: HNil)(fa: HNil) = ff
    def map[A, B](fa: HNil)(f: A => B) = fa
  }

  implicit def mkApplyConst[T](implicit T: Semigroup[T]): MkApply[Const[T]#λ] =
    new MkApply[Const[T]#λ] {
      def ap[A, B](ff: T)(fa: T) = T.combine(ff, fa)
      def map[A, B](fa: T)(f: A => B) = fa
    }
}

abstract private[derived] class MkApplyNested extends MkApplyGeneric {

  implicit def mkApplyNested[F[_]](implicit F: Split1[F, ApplyOrMk, ApplyOrMk]): MkApply[F] =
    new MkApply[F] {

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val fo = F.fo.unify
        F.pack(fo.ap(fo.map(F.unpack(ff))(fif => F.fi.unify.ap(fif)(_: F.I[A])))(F.unpack(fa)))
      }

      def map[A, B](fa: F[A])(f: A => B) =
        F.pack(F.fo.unify.map(F.unpack(fa))(F.fi.unify.map(_)(f)))
    }
}

abstract private[derived] class MkApplyGeneric {
  protected type ApplyOrMk[F[_]] = Apply[F] OrElse MkApply[F]

  implicit def mkApplyHCons[F[_]](implicit F: IsHCons1[F, ApplyOrMk, MkApply]): MkApply[F] =
    new MkApply[F] {

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val (fhf, ftf) = F.unpack(ff)
        val (fha, fta) = F.unpack(fa)
        F.pack(F.fh.unify.ap(fhf)(fha), F.ft.ap(ftf)(fta))
      }

      def map[A, B](fa: F[A])(f: A => B) = {
        val (fha, fta) = F.unpack(fa)
        F.pack(F.fh.unify.map(fha)(f), F.ft.map(fta)(f))
      }
    }

  implicit def mkApplyGeneric[F[_]](implicit F: Generic1[F, MkApply]): MkApply[F] =
    new MkApply[F] {
      def ap[A, B](ff: F[A => B])(fa: F[A]) = F.from(F.fr.ap(F.to(ff))(F.to(fa)))
      def map[A, B](fa: F[A])(f: A => B) = F.from(F.fr.map(F.to(fa))(f))
    }
}
