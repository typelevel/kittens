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
import export.{ exports, imports, reexports }
import shapeless._

@reexports[MkSemigroup]
object semigroup {
  @imports[Semigroup]
  object legacy
}

trait MkSemigroup[T] extends Semigroup[T]

@exports
object MkSemigroup {
  def apply[T](implicit met: MkSemigroup[T]): MkSemigroup[T] = met

  implicit val hnil: MkSemigroup[HNil] =
    new MkSemigroup[HNil] {
      def combine(a: HNil, b: HNil) = HNil
    }

  implicit def hcons[H, T <: HList](implicit semigroupH: Lazy[Semigroup[H]], semigroupT: Lazy[MkSemigroup[T]]): MkSemigroup[H :: T] =
    new MkSemigroup[H :: T] {
      def combine(a: H :: T, b: H :: T) =
        semigroupH.value.combine(a.head, b.head) :: semigroupT.value.combine(a.tail, b.tail)
    }


  implicit def generic[T, R](
                              implicit gen: Generic.Aux[T, R], semigroupR: Lazy[MkSemigroup[R]]): MkSemigroup[T] =
    new MkSemigroup[T] {
      def combine(a: T, b: T) = gen.from(semigroupR.value.combine(gen.to(a), gen.to(b)))
    }
}
