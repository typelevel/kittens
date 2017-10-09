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

trait MkMonoidK[F[_]] extends MonoidK[F]

object MkMonoidK extends MkMonoidKDerivation {
  def apply[F[_]](implicit mk: MkMonoidK[F]): MkMonoidK[F] = mk
}

trait MkMonoidKDerivation extends MkMonoidK0 {
  implicit val mkMonoidKHnil: MkMonoidK[Const[HNil]#λ] =
    new MkMonoidK[Const[HNil]#λ] {
      def empty[A] = HNil
      def combineK[A](x: HNil, y: HNil) = HNil
    }

  implicit def mkMonoidKHcons[F[_]](implicit ihc: IsHCons1[F, MonoidK, MkMonoidK])
    : MkMonoidK[F] = new MkMonoidK[F] {
      import ihc._
      def empty[A] = pack(fh.empty, ft.empty)
      def combineK[A](x: F[A], y: F[A]) = {
        val (hx, tx) = unpack(x)
        val (hy, ty) = unpack(y)
        pack(fh.combineK(hx, hy), ft.combineK(tx, ty))
      }
    }
}

trait MkMonoidK0 extends MkMonoidK0a {
  implicit def mkMonoidKHconsFurther[F[_]](implicit ihc: IsHCons1[F, MkMonoidK, MkMonoidK])
  : MkMonoidK[F] = new MkMonoidK[F] {
    import ihc._
    def empty[A] = pack(fh.empty, ft.empty)
    def combineK[A](x: F[A], y: F[A]) = {
      val (hx, tx) = unpack(x)
      val (hy, ty) = unpack(y)
      pack(fh.combineK(hx, hy), ft.combineK(tx, ty))
    }
  }
}

trait MkMonoidK0a extends MkMonoidK1 {
  implicit def mkMonoidKComposed[F[_]](implicit split: Split1[F, MonoidK, Trivial1])
    : MkMonoidK[F] = new MkMonoidK[F] {
      import split._
      def empty[A] = pack(fo.empty[I[A]])
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.combineK(unpack(x), unpack(y)))
    }
}

trait MkMonoidK1 extends MkMonoidK1a {
  implicit def mkMonoidKComposedFurther[F[_]](implicit split: Split1[F, MkMonoidK, Trivial1])
  : MkMonoidK[F] = new MkMonoidK[F] {
    import split._
    def empty[A] = pack(fo.empty[I[A]])
    def combineK[A](x: F[A], y: F[A]) =
      pack(fo.combineK(unpack(x), unpack(y)))
  }
}

trait MkMonoidK1a extends MkMonoidK2 {
  implicit def mkMonoidKApplicative[F[_]](implicit split: Split1[F, Applicative, MonoidK])
    : MkMonoidK[F] = new MkMonoidK[F] {
      import split._
      def empty[A] = pack(fo.pure(fi.empty[A]))
      def combineK[A](x: F[A], y: F[A]) =
        pack(fo.map2(unpack(x), unpack(y))(fi.combineK(_, _)))
    }
}

trait MkMonoidK2 extends MkMonoidK2a {
  implicit def mkMonoidKApplicativeFurther[F[_]](implicit split: Split1[F, Applicative, MkMonoidK])
  : MkMonoidK[F] = new MkMonoidK[F] {
    import split._
    def empty[A] = pack(fo.pure(fi.empty[A]))
    def combineK[A](x: F[A], y: F[A]) =
      pack(fo.map2(unpack(x), unpack(y))(fi.combineK(_, _)))
  }
}

trait MkMonoidK2a extends MkMonoidK3 {
  implicit def mkMonoidKGeneric[F[_]](implicit gen: Generic1[F, MkMonoidK])
    : MkMonoidK[F] = new MkMonoidK[F] {
      import gen._
      def empty[A] = from(fr.empty)
      def combineK[A](x: F[A], y: F[A]) =
        from(fr.combineK(to(x), to(y)))
    }
}

trait MkMonoidK3 {
  implicit def mkMonoidKConst[T](implicit m: Monoid[T])
    : MkMonoidK[Const[T]#λ] = new MkMonoidK[Const[T]#λ] {
      def empty[A] = m.empty
      def combineK[A](x: T, y: T) = m.combine(x, y)
    }
}
