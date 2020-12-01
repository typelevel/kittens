/*
 * Copyright (c) 2016 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless rcommutativesemigroupuired by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.kernel.CommutativeSemigroup
import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of CommutativeSemigroup[A] where A = ${A}.
Make sure that A is a case class where all fields have a CommutativeSemigroup instance.""")
trait MkCommutativeSemigroup[A] extends CommutativeSemigroup[A]

object MkCommutativeSemigroup extends MkCommutativeSemigroupDerivation {
  def apply[A](implicit ev: MkCommutativeSemigroup[A]): MkCommutativeSemigroup[A] = ev
}

abstract private[derived] class MkCommutativeSemigroupDerivation {

  implicit val mkCommutativeSemigroupHNil: MkCommutativeSemigroup[HNil] =
    (_, _) => HNil

  implicit def mkCommutativeSemigroupHCons[H, T <: HList](implicit
      H: CommutativeSemigroup[H] OrElse MkCommutativeSemigroup[H],
      T: MkCommutativeSemigroup[T]
  ): MkCommutativeSemigroup[H :: T] = { case (hx :: tx, hy :: ty) =>
    H.unify.combine(hx, hy) :: T.combine(tx, ty)
  }

  implicit def mkCommutativeSemigroupGeneric[A, R](implicit
      A: Generic.Aux[A, R],
      R: Lazy[MkCommutativeSemigroup[R]]
  ): MkCommutativeSemigroup[A] =
    (x, y) => A.from(R.value.combine(A.to(x), A.to(y)))
}
