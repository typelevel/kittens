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

import cats.PartialOrder
import cats.derived.util.VersionSpecific.{Lazy, OrElse}
import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive PartialOrder for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form PartialOrder
  * sealed trait where all subclasses form PartialOrder""")
trait MkPartialOrder[A] extends PartialOrder[A]

object MkPartialOrder extends MkPartialOrderDerivation {
  def apply[A](implicit ev: MkPartialOrder[A]): MkPartialOrder[A] = ev
}

abstract private[derived] class MkPartialOrderDerivation extends MkPartialOrderSingletons {
  implicit val mkPartialOrderHNil: MkPartialOrder[HNil] = (_, _) => 0
  implicit val mkPartialOrderCNil: MkPartialOrder[CNil] = (_, _) => 0

  implicit def mkPartialOrderHcons[H, T <: HList](implicit
      H: PartialOrder[H] OrElse MkPartialOrder[H],
      T: MkPartialOrder[T]
  ): MkPartialOrder[H :: T] = { case (hx :: tx, hy :: ty) =>
    val cmpH = H.unify.partialCompare(hx, hy)
    if (cmpH != 0) cmpH else T.partialCompare(tx, ty)
  }

  implicit def mkPartialOrderCCons[L, R <: Coproduct](implicit
      L: PartialOrder[L] OrElse MkPartialOrder[L],
      R: MkPartialOrder[R]
  ): MkPartialOrder[L :+: R] = {
    case (Inl(lx), Inl(ly)) => L.unify.partialCompare(lx, ly)
    case (Inr(rx), Inr(ry)) => R.partialCompare(rx, ry)
    case _ => Double.NaN
  }

  implicit def mkPartialOrderGeneric[A, R](implicit
      A: Generic.Aux[A, R],
      R: Lazy[MkPartialOrder[R]]
  ): MkPartialOrder[A] = (x, y) => R.value.partialCompare(A.to(x), A.to(y))
}

abstract private[derived] class MkPartialOrderSingletons {
  implicit def mkPartialOrderSingleton[A: Witness.Aux]: MkPartialOrder[A] =
    (_, _) => 0
}
