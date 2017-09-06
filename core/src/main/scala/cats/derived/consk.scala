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

object consk {
  object exports {
    def apply[F[_]](implicit mcff: MkConsK[F, F]) =
      ConsK[F](
        new ConsK[F] {
          def cons[A](hd: A, tl: F[A]): F[A] = mcff.cons(hd, tl)
        }
      )

    implicit def deriveConsK[F[_]](implicit mcff: MkConsK[F, F]): ConsK[F] = apply[F]
  }
}

trait MkConsK[F[_], G[_]] {
  def cons[A](hd: A, tl: G[A]): F[A]
}

object MkConsK extends MkConsK0 {
  def apply[F[_], G[_]](implicit mcfg: MkConsK[F, G]): MkConsK[F, G] = mcfg
}

trait MkConsK0 extends MkConsK1 {
  implicit def hconsL[G[_]]: MkConsK[λ[t => t :: G[t] :: HNil], G] =
    new MkConsK[λ[t => t :: G[t] :: HNil], G] {
      def cons[A](hd: A, tl: G[A]): A :: G[A] :: HNil = hd :: tl :: HNil
    }

  implicit def cconsL[F[_], G[_]]
    (implicit icf: IsCCons1[F, MkConsK[?[_], G], Trivial1]): MkConsK[F, G] =
      new MkConsK[F, G] {
        def cons[A](hd: A, tl: G[A]): F[A] = {
          import icf._
          pack(Left(fh.cons(hd, tl)))
        }
      }
}

trait MkConsK1 extends MkConsK2 {
  implicit def hconsR[G[_]]: MkConsK[λ[t => G[t] :: t :: HNil], G] =
    new MkConsK[λ[t => G[t] :: t :: HNil], G] {
      def cons[A](hd: A, tl: G[A]): G[A] :: A :: HNil = tl :: hd :: HNil
    }

  implicit def cconsR[F[_], G[_]]
    (implicit icf: IsCCons1[F, Trivial1, MkConsK[?[_], G]]): MkConsK[F, G] =
      new MkConsK[F, G] {
        def cons[A](hd: A, tl: G[A]): F[A] = {
          import icf._
          pack(Right(ft.cons(hd, tl)))
        }
      }
}

trait MkConsK2 {
  implicit def generic[F[_], G[_]]
    (implicit gen: Generic1[F, MkConsK[?[_], G]]): MkConsK[F, G] =
      new MkConsK[F, G] {
        def cons[A](hd: A, tl: G[A]): F[A] = gen.from(gen.fr.cons(hd, tl))
      }
}
