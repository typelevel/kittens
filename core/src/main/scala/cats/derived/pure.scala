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

import alleycats.{Empty, Pure}
import shapeless._
import util.VersionSpecific.OrElse

import scala.annotation.implicitNotFound

@implicitNotFound("""
Could not derive an instance of Pure[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Empty
  * it is a nested type λ[x => G[H[x]]] where G: Pure and H: Pure
  * it is a generic case class where all fields have a Pure instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector
""".trim)
trait MkPure[F[_]] extends Pure[F]

object MkPure extends MkPureDerivation {
  def apply[F[_]](implicit F: MkPure[F]): MkPure[F] = F
}

private[derived] abstract class MkPureDerivation extends MkPureNested {

  implicit val mkPureHNil: MkPure[Const[HNil]#λ] =
    new MkPure[Const[HNil]#λ] {
      def pure[A](a: A) = HNil
    }

  implicit def mkPureConst[T](implicit T: Empty[T]): MkPure[Const[T]#λ] =
    new MkPure[Const[T]#λ] {
      def pure[A](a: A) = T.empty
    }
}

private[derived] abstract class MkPureNested extends MkPureCons {

  implicit def mkPureNested[F[_]](implicit F: Split1[F, PureOrMk, PureOrMk]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A) = F.pack(F.fo.unify.pure(F.fi.unify.pure(a)))
    }
}

private[derived] abstract class MkPureCons extends MkPureGeneric {

  implicit def mkPureHCons[F[_]](implicit F: IsHCons1[F, PureOrMk, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A) = F.pack((F.fh.unify.pure(a), F.ft.pure(a)))
    }
}

private[derived] abstract class MkPureGeneric {
  protected type PureOrMk[F[_]] = Pure[F] OrElse MkPure[F]

  implicit def mkPureGeneric[F[_]](implicit F: Generic1[F, MkPure]): MkPure[F] =
    new MkPure[F] {
      def pure[A](a: A) = F.from(F.fr.pure(a))
    }
}
