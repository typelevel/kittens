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

package cats.derived.util

import shapeless._

/** Summons all available instances of the typeclass `F` for members of the coproduct `C`. Unlike `LiftAll` members of
  * the coproduct without an instance will be skipped in the result.
  */
sealed trait LiftSome[F[_], C <: Coproduct] extends Serializable {
  type Out <: HList
  def instances: Out
}

object LiftSome extends LiftSomeAbsent {

  implicit def liftCNil[F[_]]: Aux[F, CNil, HNil] =
    new LiftSome[F, CNil] {
      type Out = HNil
      def instances = HNil
    }

  implicit def liftPresent[F[_], L, R <: Coproduct](implicit
      F: F[L],
      R: LiftSome[F, R]
  ): Aux[F, L :+: R, F[L] :: R.Out] = new LiftSome[F, L :+: R] {
    type Out = F[L] :: R.Out
    val instances = F :: R.instances
  }
}

abstract private[util] class LiftSomeAbsent {
  type Aux[F[_], C <: Coproduct, O <: HList] = LiftSome[F, C] { type Out = O }
  def apply[F[_], C <: Coproduct](lift: LiftSome[F, C]): Aux[F, C, lift.Out] = lift

  implicit def liftAbsent[F[_], L, R <: Coproduct](implicit
      R: LiftSome[F, R]
  ): Aux[F, L :+: R, R.Out] = new LiftSome[F, L :+: R] {
    type Out = R.Out
    def instances = R.instances
  }
}
