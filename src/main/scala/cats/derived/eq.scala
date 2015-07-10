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

/**
 * Derived [[Eq]] instances for products and coproducts via Shapeless.
 *
 * Inspired by work in [[https://github.com/typelevel/shapeless-contrib shapeless-contrib]]
 * by Miles Sabin and Lars Hupel.
 */
object EqDerivedOrphans extends TypeClassCompanion[Eq] {
  object typeClass extends TypeClass[Eq] {
    def emptyProduct = new Eq[HNil] {
      def eqv(a: HNil, b: HNil) = true
    }

    def product[H, T <: HList](eqH: Eq[H], eqT: Eq[T]) = new Eq[H :: T] {
      def eqv(a: H :: T, b: H :: T) = eqH.eqv(a.head, b.head) && eqT.eqv(a.tail, b.tail)
    }

    def emptyCoproduct: Eq[CNil] = new Eq[CNil] {
      def eqv(a: CNil, b: CNil) = true
    }

      def coproduct[L, R <: Coproduct](eqL: => Eq[L], eqR: => Eq[R]) =
        new Eq[L :+: R] {
          def eqv(a: L :+: R, b: L :+: R) = (a, b) match {
            case (Inl(l1), Inl(l2)) => eqL.eqv(l1, l2)
            case (Inr(r1), Inr(r2)) => eqR.eqv(r1, r2)
            case _ => false
          }
        }

    def project[F, G](instance: => Eq[G], to: F => G, from: G => F) = new Eq[F] {
      def eqv(a: F, b: F) = instance.eqv(to(a), to(b))
    }
  }
}
