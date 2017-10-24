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

trait MkMonoid[T] extends Monoid[T]

object MkMonoid extends MkMonoidDerivation {
  def apply[T](implicit m: MkMonoid[T]): MkMonoid[T] = m
}

private[derived] abstract class MkMonoidDerivation {
  implicit def mkMonoidAlgebraic[T](implicit e: Lazy[MkEmpty[T]], sg: Lazy[MkSemigroup[T]])
    : MkMonoid[T] = new MkMonoid[T] {
      def empty = e.value.empty
      def combine(x: T, y: T) = sg.value.combine(x, y)
    }
}
