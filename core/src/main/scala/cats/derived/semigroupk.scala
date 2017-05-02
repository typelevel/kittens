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

import cats.{ Apply, Semigroup, SemigroupK }
import export.{ exports, imports, reexports }
import shapeless._

@reexports[MkSemigroupK]
object semigroupk {
  @imports[SemigroupK]
  object legacy
}

trait MkSemigroupK[F[_]] extends SemigroupK[F]

@exports
object MkSemigroupK extends MkSemigroupK0 {
  def apply[F[_]](implicit sgk: MkSemigroupK[F]): MkSemigroupK[F] = sgk

  implicit val hnil: MkSemigroupK[Const[HNil]#λ] =
    new MkSemigroupK[Const[HNil]#λ] {
      def empty[A] = HNil
      def combineK[A](x: HNil, y: HNil) = HNil
    }

  implicit def hcons[F[_]](implicit ihc: IsHCons1[F, SemigroupK, MkSemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import ihc._
      def combineK[A](x: F[A], y: F[A]) = {
        val (hx, tx) = unpack(x)
        val (hy, ty) = unpack(y)
        pack(fh.combineK(hx, hy), ft.combineK(tx, ty))
      }
    }
}

trait MkSemigroupK0 extends MkSemigroupK1 {
  implicit def composed[F[_]](implicit split: Split1[F, SemigroupK, Trivial.PH1])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import split._
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.combineK(unpack(x), unpack(y)))
    }
}

trait MkSemigroupK1 extends MkSemigroupK2 {
  implicit def applied[F[_]](implicit split: Split1[F, Apply, SemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import split._
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.map2(unpack(x), unpack(y))(fi.combineK(_, _)))
    }
}

trait MkSemigroupK2 extends MkSemigroupK3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkSemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import gen._
      def combineK[A](x: F[A], y: F[A]) =
        from(fr.combineK(to(x), to(y)))
    }
}

trait MkSemigroupK3 {
  implicit def const[T](implicit sg: Semigroup[T])
    : MkSemigroupK[Const[T]#λ] = new MkSemigroupK[Const[T]#λ] {
      def combineK[A](x: T, y: T) = sg.combine(x, y)
    }
}
