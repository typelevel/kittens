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

trait MkPartialOrder[T] extends PartialOrder[T]

object MkPartialOrder extends MkPartialOrderDerivation {
  def apply[T](implicit met: MkPartialOrder[T]): MkPartialOrder[T] = met
}

private[derived] abstract class MkPartialOrderDerivation {
  implicit val mkPartialOrderHnil: MkPartialOrder[HNil] =
    new MkPartialOrder[HNil] {
      override def partialCompare(x: HNil, y: HNil): Double = 0
    }

  // Compare product from left to right
  implicit def mkPartialOrderHcons[H, T <: HList](implicit ordH: PartialOrder[H] OrElse MkPartialOrder[H], ordT: MkPartialOrder[T]): MkPartialOrder[H :: T] =
    new MkPartialOrder[H :: T] {
      override def partialCompare(x: H :: T, y: H :: T): Double = {
        val compareHead = ordH.unify.partialCompare(x.head, y.head)
        if(compareHead != 0)
          compareHead
        else
          ordT.partialCompare(x.tail, y.tail)
      }
    }

  implicit val mkPartialOrderCnil: MkPartialOrder[CNil] =
    new MkPartialOrder[CNil] {
      override def partialCompare(x: CNil, y: CNil): Double = 0
    }

  implicit def mkPartialOrderCcons[L, R <: Coproduct](implicit ordL: PartialOrder[L] OrElse MkPartialOrder[L], ordR: MkPartialOrder[R]): MkPartialOrder[L :+: R] = new MkPartialOrder[L :+: R] {
    override def partialCompare(x: L :+: R, y: L :+: R): Double = {
      (x, y) match {
        case (Inl(l1), Inl(l2)) => ordL.unify.partialCompare(l1, l2)
        case (Inr(r1), Inr(r2)) => ordR.partialCompare(r1, r2)
        case _ => Double.NaN
      }
    }
  }

  implicit def mkPartialOrderGeneric[T, R](implicit gen: Generic.Aux[T, R], ordR: Lazy[MkPartialOrder[R]]): MkPartialOrder[T] =
    new MkPartialOrder[T] {
      override def partialCompare(x: T, y: T): Double = ordR.value.partialCompare(gen.to(x), gen.to(y))
    }
}
