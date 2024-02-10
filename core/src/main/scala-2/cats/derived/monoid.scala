/*
 * Copyright (c) 2016 Miles Sabin
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

import cats.Monoid
import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive Monoid for ${A}.
Make sure it is a case class where all fields form Monoid.""")
trait MkMonoid[A] extends Monoid[A]

object MkMonoid extends MkMonoidDerivation {
  def apply[A](implicit ev: MkMonoid[A]): MkMonoid[A] = ev
}

abstract private[derived] class MkMonoidDerivation {

  implicit val mkMonoidHNil: MkMonoid[HNil] =
    instance[HNil](HNil)((_, _) => HNil)

  implicit def mkMonoidHCons[H, T <: HList](implicit
      H: Monoid[H] OrElse MkMonoid[H],
      T: MkMonoid[T]
  ): MkMonoid[H :: T] = instance(H.unify.empty :: T.empty) { case (hx :: tx, hy :: ty) =>
    H.unify.combine(hx, hy) :: T.combine(tx, ty)
  }

  implicit def mkMonoidGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkMonoid[R]]): MkMonoid[A] =
    new MkMonoid[A] {
      // Cache empty case classes.
      lazy val empty = A.from(R.value.empty)
      def combine(x: A, y: A) = A.from(R.value.combine(A.to(x), A.to(y)))
    }

  private def instance[A](default: => A)(f: (A, A) => A): MkMonoid[A] =
    new MkMonoid[A] {
      def empty = default
      def combine(x: A, y: A) = f(x, y)
    }
}
