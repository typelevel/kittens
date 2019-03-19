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
import shapeless._


trait MkSemigroupK[F[_]] extends SemigroupK[F]

object MkSemigroupK extends MkSemigroupK0 {
  def apply[F[_]](implicit sgk: MkSemigroupK[F]): MkSemigroupK[F] = sgk
}

private[derived] abstract class  MkSemigroupK0 extends MkSemigroupK0b {
  implicit val mkSemigroupKHnil: MkSemigroupK[Const[HNil]#λ] =
    new MkSemigroupK[Const[HNil]#λ] {
      def empty[A] = HNil
      def combineK[A](x: HNil, y: HNil) = HNil
    }

  implicit def mkSemigroupKHcons[F[_]](implicit ihc: IsHCons1[F, SemigroupK, MkSemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import ihc._
      def combineK[A](x: F[A], y: F[A]) = {
        val (hx, tx) = unpack(x)
        val (hy, ty) = unpack(y)
        pack(fh.combineK(hx, hy), ft.combineK(tx, ty))
      }
    }

  override implicit def mkSemigroupKConst[T](implicit sg: Semigroup[T]): MkSemigroupK[Const[T]#λ] =
    super[MkSemigroupK0b].mkSemigroupKConst
}

private[derived] abstract class  MkSemigroupK0b extends MkSemigroupK1 {
  implicit def mkSemigroupKHconsFurther[F[_]](implicit ihc: IsHCons1[F, MkSemigroupK, MkSemigroupK])
  : MkSemigroupK[F] = new MkSemigroupK[F] {
    import ihc._
    def combineK[A](x: F[A], y: F[A]) = {
      val (hx, tx) = unpack(x)
      val (hy, ty) = unpack(y)
      pack(fh.combineK(hx, hy), ft.combineK(tx, ty))
    }
  }
}

private[derived] abstract class  MkSemigroupK1 extends MkSemigroupK1b {
  implicit def mkSemigroupKComposed[F[_]](implicit split: Split1[F, SemigroupK, Trivial1])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import split._
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.combineK(unpack(x), unpack(y)))
    }
}

private[derived] abstract class  MkSemigroupK1b extends MkSemigroupK2 {
  implicit def mkSemigroupKComposedFuther[F[_]](implicit split: Split1[F, MkSemigroupK, Trivial1])
  : MkSemigroupK[F] = new MkSemigroupK[F] {
    import split._
    def combineK[A](x: F[A], y: F[A]) =
      pack(fo.combineK(unpack(x), unpack(y)))
  }
}

private[derived] abstract class  MkSemigroupK2 extends MkSemigroupK2b {
  implicit def mkSemigroupKApplied[F[_]](implicit split: Split1[F, Apply, SemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import split._
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.map2(unpack(x), unpack(y))(fi.combineK(_, _)))
    }
}

private[derived] abstract class  MkSemigroupK2b extends MkSemigroupK3 {
  implicit def mkSemigroupKAppliedFuther[F[_]](implicit split: Split1[F, Apply, MkSemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import split._
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.map2(unpack(x), unpack(y))(fi.combineK(_, _)))
    }
}

private[derived] abstract class  MkSemigroupK3 extends MkSemigroupK4 {
  implicit def mkSemigroupKGeneric[F[_]](implicit gen: Generic1[F, MkSemigroupK])
    : MkSemigroupK[F] = new MkSemigroupK[F] {
      import gen._
      def combineK[A](x: F[A], y: F[A]) =
        from(fr.combineK(to(x), to(y)))
    }
}

trait MkSemigroupK4 {

  // For binary compatibility.
  def mkSemigroupKConst[T](implicit sg: Semigroup[T]): MkSemigroupK[Const[T]#λ] =
    new MkSemigroupK[Const[T]#λ] {
      def combineK[A](x: T, y: T) = sg.combine(x, y)
    }
}
