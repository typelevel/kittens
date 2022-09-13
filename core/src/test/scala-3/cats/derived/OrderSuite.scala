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
import cats.kernel.laws.discipline.*
import scala.compiletime.*

class OrderSuite extends KittensSuite:
  import OrderSuite.*
  import TestDefns.*

  inline def tests[A]: OrderTests[A] =
    OrderTests[A](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[Inner]", tests[Inner].order)
    checkAll(s"$instance[Outer]", tests[Outer].order)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].order)
    checkAll(s"$instance[Recursive]", tests[Recursive].order)
    checkAll(s"$instance[GenericAdt[Int]]", tests[GenericAdt[Int]].order)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].order)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Order[Interleaved[Int]]]))

  locally {
    import auto.order.given
    validate("auto.order")
  }

  locally {
    import semiInstances.given
    validate("semiauto.order")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.order"
    checkAll(s"$instance[Inner]", tests[Inner].order)
    checkAll(s"$instance[Outer]", tests[Outer].order)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].order)
    checkAll(s"$instance[Recursive]", tests[Recursive].order)
    checkAll(s"$instance[GenericAdt[Int]]", tests[GenericAdt[Int]].order)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].order)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Order[Interleaved[Int]]]))
  }

end OrderSuite

object OrderSuite:
  import TestDefns.*

  object semiInstances:
    given Order[Inner] = semiauto.order
    given Order[Outer] = semiauto.order
    given Order[Interleaved[Int]] = semiauto.order
    given Order[Recursive] = semiauto.order
    given Order[GenericAdt[Int]] = semiauto.order
    given Order[EnumK0] = semiauto.order

  object derivedInstances:
    case class Inner(x: TestDefns.Inner) derives Order
    case class Outer(x: TestDefns.Outer) derives Order
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Order
    case class Recursive(x: TestDefns.Recursive) derives Order
    case class GenericAdt[A](x: TestDefns.GenericAdt[A]) derives Order
    case class EnumK0(x: TestDefns.EnumK0) derives Order

end OrderSuite