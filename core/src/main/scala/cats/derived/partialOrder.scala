/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless rorduired by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.PartialOrder
import shapeless._
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""
Could not derive an instance of PartialOrder[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have a PartialOrder instance
  * it is a sealed trait where all subclasses have a PartialOrder instance
""".trim)
trait MkPartialOrder[A] extends PartialOrder[A]

object MkPartialOrder extends MkPartialOrderDerivation {
  def apply[A](implicit ev: MkPartialOrder[A]): MkPartialOrder[A] = ev
}

private[derived] abstract class MkPartialOrderDerivation {
  implicit val mkPartialOrderHNil: MkPartialOrder[HNil] = instance((_, _) => 0)
  implicit val mkPartialOrderCNil: MkPartialOrder[CNil] = instance((_, _) => 0)

  implicit def mkPartialOrderHcons[H, T <: HList](
    implicit H: PartialOrder[H] OrElse MkPartialOrder[H], T: MkPartialOrder[T]
  ): MkPartialOrder[H :: T] = instance { case (hx :: tx, hy :: ty) =>
    val cmpH = H.unify.partialCompare(hx, hy)
    if (cmpH != 0) cmpH else T.partialCompare(tx, ty)
  }

  implicit def mkPartialOrderCCons[L, R <: Coproduct](
    implicit L: PartialOrder[L] OrElse MkPartialOrder[L], R: MkPartialOrder[R]
  ): MkPartialOrder[L :+: R] = instance {
    case (Inl(lx), Inl(ly)) => L.unify.partialCompare(lx, ly)
    case (Inr(rx), Inr(ry)) => R.partialCompare(rx, ry)
    case _ => Double.NaN
  }

  implicit def mkPartialOrderGeneric[A, R](
    implicit A: Generic.Aux[A, R], R: Lazy[MkPartialOrder[R]]
  ): MkPartialOrder[A] = instance((x, y) => R.value.partialCompare(A.to(x), A.to(y)))

  private def instance[A](f: (A, A) => Double): MkPartialOrder[A] =
    new MkPartialOrder[A] {
      def partialCompare(x: A, y: A) = f(x, y)
    }
}
