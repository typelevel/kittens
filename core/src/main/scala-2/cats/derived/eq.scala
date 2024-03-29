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
import cats.derived.util.VersionSpecific.{Lazy, OrElse}
import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive Eq for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Eq
  * sealed trait where all subclasses form Eq""")
trait MkEq[A] extends Eq[A]

object MkEq extends MkEqDerivation {
  def apply[A](implicit ev: MkEq[A]): MkEq[A] = ev
}

abstract private[derived] class MkEqDerivation extends MkEqSingletons {
  implicit val mkEqHNil: MkEq[HNil] = (_, _) => true
  implicit val mkEqCNil: MkEq[CNil] = (_, _) => true

  implicit def mkEqHCons[H, T <: HList](implicit H: Eq[H] OrElse MkEq[H], T: MkEq[T]): MkEq[H :: T] = {
    case (hx :: tx, hy :: ty) => H.unify.eqv(hx, hy) && T.eqv(tx, ty)
  }

  implicit def mkEqCCons[L, R <: Coproduct](implicit L: Eq[L] OrElse MkEq[L], R: MkEq[R]): MkEq[L :+: R] = {
    case (Inl(lx), Inl(ly)) => L.unify.eqv(lx, ly)
    case (Inr(rx), Inr(ry)) => R.eqv(rx, ry)
    case _ => false
  }

  implicit def mkEqGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkEq[R]]): MkEq[A] =
    (x, y) => R.value.eqv(A.to(x), A.to(y))
}

abstract private[derived] class MkEqSingletons {
  implicit def mkEqSingleton[A: Witness.Aux]: MkEq[A] =
    (_, _) => true
}
