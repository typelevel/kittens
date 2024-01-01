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

import cats.{Applicative, Monoid, MonoidK}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of MonoidK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Monoid
  * it is a nested type λ[x => G[H[x]]] where G: MonoidK
  * it is a nested type λ[x => G[H[x]]] where G: Applicative and H: MonoidK
  * it is a generic case class where all fields have a MonoidK instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
trait MkMonoidK[F[_]] extends MonoidK[F]

object MkMonoidK extends MkMonoidKDerivation {
  def apply[F[_]](implicit F: MkMonoidK[F]): MkMonoidK[F] = F
}

abstract private[derived] class MkMonoidKDerivation extends MkMonoidKNestedOuter {

  implicit val mkMonoidKHNil: MkMonoidK[Const[HNil]#λ] =
    new MkMonoidK[Const[HNil]#λ] {
      def empty[A] = HNil
      def combineK[A](x: HNil, y: HNil) = HNil
    }

  implicit def mkMonoidKConst[T](implicit T: Monoid[T]): MkMonoidK[Const[T]#λ] =
    new MkMonoidK[Const[T]#λ] {
      def empty[A] = T.empty
      def combineK[A](x: T, y: T) = T.combine(x, y)
    }
}

abstract private[derived] class MkMonoidKNestedOuter extends MkMonoidKNestedInner {

  implicit def mkMonoidKNestedOuter[F[_]](implicit F: Split1[F, MonoidKOrMk, Trivial1]): MkMonoidK[F] =
    new MkMonoidK[F] {
      def empty[A] = F.pack(F.fo.unify.empty[F.I[A]])
      def combineK[A](x: F[A], y: F[A]) = F.pack(F.fo.unify.combineK(F.unpack(x), F.unpack(y)))
    }
}

abstract private[derived] class MkMonoidKNestedInner extends MkMonoidKGeneric {

  implicit def mkMonoidKNestedInner[F[_]](implicit F: Split1[F, Applicative, MonoidKOrMk]): MkMonoidK[F] =
    new MkMonoidK[F] {
      def empty[A] = F.pack(F.fo.pure(F.fi.unify.empty[A]))
      def combineK[A](x: F[A], y: F[A]) = F.pack(F.fo.map2(F.unpack(x), F.unpack(y))(F.fi.unify.combineK(_, _)))
    }
}

abstract private[derived] class MkMonoidKGeneric {
  protected type MonoidKOrMk[F[_]] = MonoidK[F] OrElse MkMonoidK[F]

  implicit def mkMonoidKHcons[F[_]](implicit F: IsHCons1[F, MonoidKOrMk, MkMonoidK]): MkMonoidK[F] =
    new MkMonoidK[F] {
      def empty[A] = F.pack((F.fh.unify.empty, F.ft.empty))

      def combineK[A](x: F[A], y: F[A]) = {
        val (fhx, ftx) = F.unpack(x)
        val (fhy, fty) = F.unpack(y)
        F.pack((F.fh.unify.combineK(fhx, fhy), F.ft.combineK(ftx, fty)))
      }
    }

  implicit def mkMonoidKGeneric[F[_]](implicit F: Generic1[F, MkMonoidK]): MkMonoidK[F] =
    new MkMonoidK[F] {
      def empty[A] = F.from(F.fr.empty)
      def combineK[A](x: F[A], y: F[A]) = F.from(F.fr.combineK(F.to(x), F.to(y)))
    }
}
