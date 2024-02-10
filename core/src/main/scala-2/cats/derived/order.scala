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

import cats.Order
import cats.derived.util.VersionSpecific.{Lazy, OrElse}
import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive Order for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Order
  * sealed trait with exactly one subclass that forms Order""")
trait MkOrder[A] extends Order[A]

object MkOrder extends MkOrderDerivation {
  def apply[A](implicit ev: MkOrder[A]): MkOrder[A] = ev
}

abstract private[derived] class MkOrderDerivation extends MkOrderSingletons {
  implicit val mkOrderHNil: MkOrder[HNil] = (_, _) => 0
  implicit val mkOrderCNil: MkOrder[CNil] = (_, _) => 0

  implicit def mkOrderHCons[H, T <: HList](implicit
      H: Order[H] OrElse MkOrder[H],
      T: MkOrder[T]
  ): MkOrder[H :: T] = { case (hx :: tx, hy :: ty) =>
    val cmpH = H.unify.compare(hx, hy)
    if (cmpH != 0) cmpH else T.compare(tx, ty)
  }

  implicit def mkOrderCCons[L](implicit L: Order[L] OrElse MkOrder[L]): MkOrder[L :+: CNil] = {
    case (Inl(lx), Inl(ly)) => L.unify.compare(lx, ly)
    case _ => 0
  }

  implicit def mkOrderGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkOrder[R]]): MkOrder[A] =
    (x, y) => R.value.compare(A.to(x), A.to(y))
}

abstract private[derived] class MkOrderSingletons {
  implicit def mkOrderSingleton[A: Witness.Aux]: MkOrder[A] =
    (_, _) => 0
}
