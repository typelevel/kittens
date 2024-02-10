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

import alleycats.{Empty, EmptyK, Pure}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive EmptyK for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type λ[x => T] where T: Empty
  * nested type λ[x => G[H[x]]] where G: EmptyK
  * nested type λ[x => G[H[x]]] where G: Pure and H: EmptyK
  * generic case class where all fields form EmptyK""")
trait MkEmptyK[F[_]] extends EmptyK[F]

object MkEmptyK extends MkEmptyKDerivation {
  def apply[F[_]](implicit F: MkEmptyK[F]): MkEmptyK[F] = F
}

abstract private[derived] class MkEmptyKDerivation extends MkEmptyKNestedOuter {

  implicit val mkEmptyKHNil: MkEmptyK[Const[HNil]#λ] =
    new MkEmptyK[Const[HNil]#λ] {
      def empty[A] = HNil
    }

  implicit def mkEmptyKConst[T](implicit T: Empty[T]): MkEmptyK[Const[T]#λ] =
    new MkEmptyK[Const[T]#λ] {
      def empty[A] = T.empty
    }
}

abstract private[derived] class MkEmptyKNestedOuter extends MkEmptyKNestedInner {

  implicit def mkEmptyKNestedOuter[F[_]](implicit F: Split1[F, EmptyKOrMk, Trivial1]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A] = F.pack(F.fo.unify.empty)
    }
}

abstract private[derived] class MkEmptyKNestedInner extends MkEmptyKCons {

  implicit def mkEmptyKNestedInner[F[_]](implicit F: Split1[F, Pure, EmptyKOrMk]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A] = F.pack(F.fo.pure(F.fi.unify.empty))
    }
}

abstract private[derived] class MkEmptyKCons extends MkEmptyKGeneric {

  implicit def mkEmptyKHCons[F[_]](implicit F: IsHCons1[F, EmptyKOrMk, MkEmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A] = F.pack((F.fh.unify.empty, F.ft.empty))
    }
}

abstract private[derived] class MkEmptyKGeneric {
  protected type EmptyKOrMk[F[_]] = EmptyK[F] OrElse MkEmptyK[F]

  implicit def mkEmptyKGeneric[F[_]](implicit F: Generic1[F, MkEmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A] = F.from(F.fr.empty)
    }
}
