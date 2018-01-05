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

package cats
package derived

import alleycats.Empty
import shapeless._
import scala.annotation.implicitNotFound


@implicitNotFound("Could not derive an instance of Empty[${A}]")
trait MkEmpty[A] extends Empty[A]

object MkEmpty extends MkEmptyDerivation {
  def apply[A](implicit empty: MkEmpty[A]): MkEmpty[A] = empty
}

private[derived] abstract class MkEmptyDerivation {

  protected def instance[A](default: => A): MkEmpty[A] = new MkEmpty[A] {
    def empty = default
  }

  implicit val mkEmptyHNil: MkEmpty[HNil] =
    instance(HNil)

  implicit def mkEmptyHCons[H, T <: HList](
    implicit H: Empty[H] OrElse MkEmpty[H], T: MkEmpty[T]
  ): MkEmpty[H :: T] = instance {
    H.unify.empty :: T.empty
  }

  implicit def mkEmptyGeneric[A, R](
    implicit gen: Generic.Aux[A, R], R: Lazy[MkEmpty[R]]
  ): MkEmpty[A] = new MkEmpty[A] {
    // Cache empty case classes.
    lazy val empty = gen.from(R.value.empty)
  }
}
