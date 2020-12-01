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
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive an instance of Eq[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Eq instance
  * it is a sealed trait where all subclasses have an Eq instance""")
trait MkEq[A] extends Eq[A]

object MkEq extends MkEqDerivation {
  def apply[A](implicit ev: MkEq[A]): MkEq[A] = ev
}

abstract private[derived] class MkEqDerivation {
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
