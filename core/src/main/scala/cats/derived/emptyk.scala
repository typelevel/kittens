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
import export.{ exports, reexports }
import shapeless._

@reexports[MkEmptyK]
object emptyk

trait MkEmptyK[F[_]] extends EmptyK[F]

@exports
object MkEmptyK extends MkEmptyK0 {
  def apply[F[_]](implicit mef: MkEmptyK[F]): MkEmptyK[F] = mef

  implicit val hnil: MkEmptyK[Const[HNil]#λ] =
    new MkEmptyK[Const[HNil]#λ] {
      def empty[A]: HNil = HNil
    }

  implicit def hcons[F[_]](implicit ihf: IsHCons1[F, EmptyK, MkEmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = {
        import ihf._
        pack((fh.empty, ft.empty))
      }
    }

  implicit def ccons0[F[_]](implicit icf: IsCCons1[F, EmptyK, Trivial1]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = {
        import icf._
        pack(Left(fh.empty))
      }
    }
}

trait MkEmptyK0 extends MkEmptyK1 {
  implicit def ccons1[F[_]](implicit icf: IsCCons1[F, Trivial1, MkEmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = {
        import icf._
        pack(Right(ft.empty))
      }
    }
}

trait MkEmptyK1 extends MkEmptyK2 {
  implicit def split0[F[_]](implicit split: Split1[F, EmptyK, Trivial1]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = {
        import split._
        pack(fo.empty)
      }
    }
}

trait MkEmptyK2 extends MkEmptyK3 {
  implicit def split1[F[_]](implicit split: Split1[F, Pure, EmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = {
        import split._
        pack(fo.pure(fi.empty))
      }
    }
}

trait MkEmptyK3 {
  implicit def generic[F[_]](implicit gen: Generic1[F, MkEmptyK]): MkEmptyK[F] =
    new MkEmptyK[F] {
      def empty[A]: F[A] = gen.from(gen.fr.empty)
    }
}
