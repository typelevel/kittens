/*
 * Copyright (c) 2016 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless rsemigroupuired by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.Semigroup
import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive Semigroup for ${A}.
Make sure it is a case class where all fields form Semigroup.""")
trait MkSemigroup[A] extends Semigroup[A]

object MkSemigroup extends MkSemigroupDerivation {
  def apply[A](implicit ev: MkSemigroup[A]): MkSemigroup[A] = ev
}

abstract private[derived] class MkSemigroupDerivation {

  implicit val mkSemigroupHNil: MkSemigroup[HNil] =
    (_, _) => HNil

  implicit def mkSemigroupHCons[H, T <: HList](implicit
      H: Semigroup[H] OrElse MkSemigroup[H],
      T: MkSemigroup[T]
  ): MkSemigroup[H :: T] = { case (hx :: tx, hy :: ty) =>
    H.unify.combine(hx, hy) :: T.combine(tx, ty)
  }

  implicit def mkSemigroupGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkSemigroup[R]]): MkSemigroup[A] =
    (x, y) => A.from(R.value.combine(A.to(x), A.to(y)))
}
