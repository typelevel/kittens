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

import cats.{ EmptyK, Pointed }
import shapeless._

object pointed {
  implicit def apply[F[_]](implicit mpf: WrappedOrphan[MkPointed[F]]): Pointed[F] = mpf.instance
}

trait MkPointed[F[_]] extends Pointed[F]

object MkPointed extends MkPointed0 {
  def apply[F[_]](implicit mef: MkPointed[F]): MkPointed[F] = mef

  implicit def emptyk[F[_]](implicit cf: Pointed[F]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = cf.point(a)
    }
}

trait MkPointed0 extends MkPointed1 {
  implicit def hcons0[F[_]](implicit ihf: IsHCons1[F, MkPointed, MkEmptyK]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = {
        import ihf._
        pack((fh.point(a), ft.empty))
      }
    }

  implicit def ccons0[F[_]](implicit icf: IsCCons1[F, MkPointed, Trivial1]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = {
        import icf._
        pack(Left(fh.point(a)))
      }
    }
}

trait MkPointed1 extends MkPointed2 {
  implicit def hcons1[F[_]](implicit ihf: IsHCons1[F, MkEmptyK, MkPointed]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = {
        import ihf._
        pack((fh.empty, ft.point(a)))
      }
    }

  implicit def ccons1[F[_]](implicit icf: IsCCons1[F, Trivial1, MkPointed]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = {
        import icf._
        pack(Right(ft.point(a)))
      }
    }
}

trait MkPointed2 extends MkPointed3 {
  implicit def split[F[_]](implicit split: Split1[F, MkPointed, MkPointed]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = {
        import split._
        pack(fo.point(fi.point(a)))
      }
    }
}

trait MkPointed3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkPointed]): MkPointed[F] =
    new MkPointed[F] {
      def point[A](a: A): F[A] = gen.from(gen.fr.point(a))
    }
}
