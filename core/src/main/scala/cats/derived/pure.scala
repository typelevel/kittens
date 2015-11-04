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

import alleycats.{ EmptyK, Pure }
import shapeless._

object pure {
  implicit def apply[F[_]](implicit mpf: WrappedOrphan[MkPure[F]]): Pure[F] = mpf.instance
}

trait MkPure[F[_]] extends Pure[F]

object MkPure extends MkPure0 {
  def apply[F[_]](implicit mef: MkPure[F]): MkPure[F] = mef

  implicit def emptyk[F[_]](implicit cf: Pure[F]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = cf.pure(a)
    }
}

trait MkPure0 extends MkPure1 {
  implicit def hcons0[F[_]](implicit ihf: IsHCons1[F, MkPure, MkEmptyK]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = {
        import ihf._
        pack((fh.pure(a), ft.empty))
      }
    }

  implicit def ccons0[F[_]](implicit icf: IsCCons1[F, MkPure, Trivial1]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = {
        import icf._
        pack(Left(fh.pure(a)))
      }
    }
}

trait MkPure1 extends MkPure2 {
  implicit def hcons1[F[_]](implicit ihf: IsHCons1[F, MkEmptyK, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = {
        import ihf._
        pack((fh.empty, ft.pure(a)))
      }
    }

  implicit def ccons1[F[_]](implicit icf: IsCCons1[F, Trivial1, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = {
        import icf._
        pack(Right(ft.pure(a)))
      }
    }
}

trait MkPure2 extends MkPure3 {
  implicit def split[F[_]](implicit split: Split1[F, MkPure, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = {
        import split._
        pack(fo.pure(fi.pure(a)))
      }
    }
}

trait MkPure3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A): F[A] = gen.from(gen.fr.pure(a))
    }
}
