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

import alleycats.ConsK
import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("Could not derive ConsK for ${F}")
trait MkConsK[F[_], G[_]] extends Serializable {
  def cons[A](head: A, tail: G[A]): F[A]
}

object MkConsK extends MkConsKDerivation {
  def apply[F[_], G[_]](implicit ev: MkConsK[F, G]): MkConsK[F, G] = ev

  def consK[F[_]](implicit F: MkConsK[F, F]): ConsK[F] =
    new ConsK[F] {
      def cons[A](head: A, tail: F[A]) = F.cons(head, tail)
    }
}

abstract private[derived] class MkConsKDerivation extends MkConsKRight {

  implicit def mkConsKHConsLeft[G[_]]: MkConsK[λ[t => t :: G[t] :: HNil], G] =
    new MkConsK[λ[t => t :: G[t] :: HNil], G] {
      def cons[A](head: A, tail: G[A]) = head :: tail :: HNil
    }

  implicit def mkConsKCConsLeft[F[_], G[_]](implicit F: IsCCons1[F, MkConsK[*[_], G], Trivial1]): MkConsK[F, G] =
    new MkConsK[F, G] {
      def cons[A](head: A, tail: G[A]) = F.pack(Left(F.fh.cons(head, tail)))
    }
}

abstract private[derived] class MkConsKRight extends MkConsKGeneric {

  implicit def mkConsKHConsRight[G[_]]: MkConsK[λ[t => G[t] :: t :: HNil], G] =
    new MkConsK[λ[t => G[t] :: t :: HNil], G] {
      def cons[A](head: A, tail: G[A]) = tail :: head :: HNil
    }

  implicit def mkConsKCConsRight[F[_], G[_]](implicit F: IsCCons1[F, Trivial1, MkConsK[*[_], G]]): MkConsK[F, G] =
    new MkConsK[F, G] {
      def cons[A](head: A, tail: G[A]) = F.pack(Right(F.ft.cons(head, tail)))
    }
}

abstract private[derived] class MkConsKGeneric {

  implicit def mkConsKGeneric[F[_], G[_]](implicit F: Generic1[F, MkConsK[*[_], G]]): MkConsK[F, G] =
    new MkConsK[F, G] {
      def cons[A](head: A, tail: G[A]) = F.from(F.fr.cons(head, tail))
    }
}
