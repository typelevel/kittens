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

import cats.Order
import shapeless._

trait MkOrder[T] extends Order[T]

object MkOrder extends MkOrderDerivation {
  def apply[T](implicit met: MkOrder[T]): MkOrder[T] = met
}

private[derived] abstract class MkOrderDerivation {
  implicit val mkOrderHnil: MkOrder[HNil] =
    new MkOrder[HNil] {
      override def compare(x: HNil, y: HNil): Int = 0
    }

  // Compare product from left to right
  implicit def mkOrderHcons[H, T <: HList](implicit ordH: Order[H] OrElse MkOrder[H], ordT: MkOrder[T]): MkOrder[H :: T] =
    new MkOrder[H :: T] {
      override def compare(x: H :: T, y: H :: T): Int = {
        val compareHead = ordH.unify.compare(x.head, y.head)
        if(compareHead != 0)
          compareHead
        else
          ordT.compare(x.tail, y.tail)
      }
    }
  implicit val mkOrderCnil: MkOrder[CNil] =
    new MkOrder[CNil] {
      override def compare(x: CNil, y: CNil): Int = 0
    }

  implicit def mkOrderCcons[L, R <: Coproduct](implicit ordL: Order[L] OrElse MkOrder[L], ordR: MkOrder[R]): MkOrder[L :+: R] = new MkOrder[L :+: R] {
    def compare(x: L :+: R, y: L :+: R): Int = {
      (x, y) match {
        case (Inl(l1), Inl(l2)) => ordL.unify.compare(l1, l2)
        case (Inr(r1), Inr(r2)) => ordR.compare(r1, r2)
        case (Inl(_), Inr(_)) => -1
        case _  => 1
      }
    }
  }


  implicit def mkOrderGeneric[T, R](implicit gen: Generic.Aux[T, R], ordR: Lazy[MkOrder[R]]): MkOrder[T] =
    new MkOrder[T] {
      override def compare(x: T, y: T): Int = ordR.value.compare(gen.to(x), gen.to(y))
    }
}
