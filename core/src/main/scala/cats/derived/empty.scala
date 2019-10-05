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
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of Empty[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Empty instance
  * it is a sealed trait where exactly one subclass has an Empty instance""")
trait MkEmpty[A] extends Empty[A]

object MkEmpty extends MkEmptyDerivation {
  def apply[A](implicit ev: MkEmpty[A]): MkEmpty[A] = ev
}

private[derived] abstract class MkEmptyDerivation {
  protected type EmptyOrMk[A] = Empty[A] OrElse MkEmpty[A]

  implicit val mkEmptyHNil: MkEmpty[HNil] =
    instance(HNil)

  implicit def mkEmptyHCons[H, T <: HList](implicit H: EmptyOrMk[H], T: MkEmpty[T]): MkEmpty[H :: T] =
    instance(H.unify.empty :: T.empty)

  implicit def mkEmptyCoproduct[C <: Coproduct, E <: HList, A](
    implicit lift: util.LiftSome.Aux[EmptyOrMk, C, E],
    unique: E <:< (EmptyOrMk[A] :: HNil),
    inject: ops.coproduct.Inject[C, A]
  ): MkEmpty[C] = instance(inject(lift.instances.head.unify.empty))

  implicit def mkEmptyGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkEmpty[R]]): MkEmpty[A] =
    new MkEmpty[A] {
      // Cache empty case classes and sealed traits.
      lazy val empty = A.from(R.value.empty)
    }

  private def instance[A](default: A): MkEmpty[A] =
    new MkEmpty[A] {
      def empty = default
    }
}
