/*
 * Copyright (c) 2015 Miles Sabin
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

import cats.Eq
import shapeless._


trait MkEq[T] extends Eq[T]

object MkEq extends MkEqDerivation {
  def apply[T](implicit met: MkEq[T]): MkEq[T] = met
}

private[derived] abstract class MkEqDerivation {
  implicit val mkEqHnil: MkEq[HNil] =
    new MkEq[HNil] {
      def eqv(a: HNil, b: HNil) = true
    }

  implicit def mkEqHcons[H, T <: HList](implicit eqH: Eq[H] OrElse MkEq[H], eqT: MkEq[T]): MkEq[H :: T] =
    new MkEq[H :: T] {
      def eqv(a: H :: T, b: H :: T) =
        eqH.unify.eqv(a.head, b.head) && eqT.eqv(a.tail, b.tail)
    }

  implicit val mkEqCnil: MkEq[CNil] =
    new MkEq[CNil] {
      def eqv(a: CNil, b: CNil) = true
    }

  implicit def mkEqCcons[L, R <: Coproduct](implicit eqL: Eq[L] OrElse MkEq[L], eqR: MkEq[R]): MkEq[L :+: R] = new MkEq[L :+: R] {
    def eqv(a: L :+: R, b: L :+: R) = {
      (a, b) match {
        case (Inl(l1), Inl(l2)) => eqL.unify.eqv(l1, l2)
        case (Inr(r1), Inr(r2)) => eqR.eqv(r1, r2)
        case _ => false
      }
    }
  }

  implicit def mkEqGeneric[T, R](implicit gen: Generic.Aux[T, R], eqR: Lazy[MkEq[R]]): MkEq[T] =
    new MkEq[T] {
      def eqv(a: T, b: T) = eqR.value.eqv(gen.to(a), gen.to(b))
    }
}
