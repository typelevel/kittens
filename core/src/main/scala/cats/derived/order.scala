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
import util.VersionSpecific.{OrElse, Lazy}

import scala.annotation.implicitNotFound

@implicitNotFound("""
Could not derive an instance of Order[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Order instance
  * it is a sealed trait with exactly one subclass that has an Order instance
""".trim)
trait MkOrder[A] extends Order[A]

object MkOrder extends MkOrderDerivation {
  def apply[A](implicit ev: MkOrder[A]): MkOrder[A] = ev
}

private[derived] abstract class MkOrderDerivation {
  implicit val mkOrderHNil: MkOrder[HNil] = instance((_, _) => 0)
  implicit val mkOrderCNil: MkOrder[CNil] = instance((_, _) => 0)

  implicit def mkOrderHCons[H, T <: HList](
    implicit H: Order[H] OrElse MkOrder[H], T: MkOrder[T]
  ): MkOrder[H :: T] = instance { case (hx :: tx, hy :: ty) =>
    val cmpH = H.unify.compare(hx, hy)
    if (cmpH != 0) cmpH else T.compare(tx, ty)
  }

  implicit def mkOrderCCons[L](implicit L: Order[L] OrElse MkOrder[L]): MkOrder[L :+: CNil] =
    instance {
      case (Inl(lx), Inl(ly)) => L.unify.compare(lx, ly)
      case _ => 0
    }

  implicit def mkOrderGeneric[A, R](implicit A: Generic.Aux[A, R], R: Lazy[MkOrder[R]]): MkOrder[A] =
    instance((x, y) => R.value.compare(A.to(x), A.to(y)))

  private def instance[A](f: (A, A) => Int): MkOrder[A] =
    new MkOrder[A] {
      def compare(x: A, y: A) = f(x, y)
    }
}
