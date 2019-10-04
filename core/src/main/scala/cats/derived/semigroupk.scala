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

import cats.{Apply, Semigroup, SemigroupK}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of SemigroupK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Semigroup
  * it is a nested type λ[x => G[H[x]]] where G: SemigroupK
  * it is a nested type λ[x => G[H[x]]] where G: Apply and H: SemigroupK
  * it is a generic case class where all fields have a SemigroupK instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkSemigroupK[F[_]] extends SemigroupK[F]

object MkSemigroupK extends MkSemigroupKDerivation {
  def apply[F[_]](implicit F: MkSemigroupK[F]): MkSemigroupK[F] = F
}

private[derived] abstract class MkSemigroupKDerivation extends MkSemigroupKNestedOuter {

  implicit val mkSemigroupKHNil: MkSemigroupK[Const[HNil]#λ] =
    new MkSemigroupK[Const[HNil]#λ] {
      def combineK[A](x: HNil, y: HNil) = HNil
    }

  implicit def mkSemigroupKConst[T](implicit T: Semigroup[T]): MkSemigroupK[Const[T]#λ] =
    new MkSemigroupK[Const[T]#λ] {
      def combineK[A](x: T, y: T) = T.combine(x, y)
    }
}

private[derived] abstract class MkSemigroupKNestedOuter extends MkSemigroupKNestedInner {

  implicit def mkSemigroupKNestedOuter[F[_]](implicit F: Split1[F, SemigroupKOrMk, Trivial1]): MkSemigroupK[F] =
    new MkSemigroupK[F] {

      def combineK[A](x: F[A], y: F[A]) =
        F.pack(F.fo.unify.combineK(F.unpack(x), F.unpack(y)))
    }
}

private[derived] abstract class  MkSemigroupKNestedInner extends MkSemigroupKGeneric {

  implicit def mkSemigroupKNestedInner[F[_]](implicit F: Split1[F, Apply, SemigroupKOrMk]): MkSemigroupK[F] =
    new MkSemigroupK[F] {

      def combineK[A](x: F[A], y: F[A]) =
        F.pack(F.fo.map2(F.unpack(x), F.unpack(y))(F.fi.unify.combineK(_, _)))
    }
}

private[derived] abstract class MkSemigroupKGeneric {
  protected type SemigroupKOrMk[F[_]] = SemigroupK[F] OrElse MkSemigroupK[F]

  implicit def mkSemigroupKHCons[F[_]](implicit F: IsHCons1[F, SemigroupKOrMk, MkSemigroupK]): MkSemigroupK[F] =
    new MkSemigroupK[F] {

      def combineK[A](x: F[A], y: F[A]) = {
        val (fhx, ftx) = F.unpack(x)
        val (fhy, fty) = F.unpack(y)
        F.pack(F.fh.unify.combineK(fhx, fhy), F.ft.combineK(ftx, fty))
      }
    }

  implicit def mkSemigroupKGeneric[F[_]](implicit F: Generic1[F, MkSemigroupK]): MkSemigroupK[F] =
    new MkSemigroupK[F] {

      def combineK[A](x: F[A], y: F[A]) =
        F.from(F.fr.combineK(F.to(x), F.to(y)))
    }
}
