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

import cats.kernel.CommutativeMonoid
import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of CommutativeMonoid[A] where A = ${A}.
Make sure that A is a case class where all fields have a CommutativeMonoid instance.""")
trait MkCommutativeMonoid[A] extends CommutativeMonoid[A]

object MkCommutativeMonoid extends MkCommutativeMonoidDerivation {
  def apply[A](implicit ev: MkCommutativeMonoid[A]): MkCommutativeMonoid[A] = ev
}

abstract private[derived] class MkCommutativeMonoidDerivation {

  implicit val mkCommutativeMonoidHNil: MkCommutativeMonoid[HNil] =
    instance[HNil](HNil)((_, _) => HNil)

  implicit def mkCommutativeMonoidHCons[H, T <: HList](implicit
      H: CommutativeMonoid[H] OrElse MkCommutativeMonoid[H],
      T: MkCommutativeMonoid[T]
  ): MkCommutativeMonoid[H :: T] = instance(H.unify.empty :: T.empty) { case (hx :: tx, hy :: ty) =>
    H.unify.combine(hx, hy) :: T.combine(tx, ty)
  }

  implicit def mkCommutativeMonoidGeneric[A, R](implicit
      A: Generic.Aux[A, R],
      R: Lazy[MkCommutativeMonoid[R]]
  ): MkCommutativeMonoid[A] =
    new MkCommutativeMonoid[A] {
      // Cache empty case classes.
      lazy val empty = A.from(R.value.empty)
      def combine(x: A, y: A) = A.from(R.value.combine(A.to(x), A.to(y)))
    }

  private def instance[A](default: => A)(f: (A, A) => A): MkCommutativeMonoid[A] =
    new MkCommutativeMonoid[A] {
      def empty = default
      def combine(x: A, y: A) = f(x, y)
    }
}
