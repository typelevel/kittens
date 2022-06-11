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
import cats.kernel.laws.discipline.{OrderTests, SerializableTests}
import org.scalacheck.Arbitrary
import scala.compiletime.summonInline

class OrderSuite extends KittensSuite {
  import OrderSuite._
  import TestDefns._

  inline def orderTests[A]: OrderTests[A] =
    OrderTests[A](summonInline)

  inline def testOrder(context: String): Unit = {
    checkAll(s"$context.Order[Inner]", orderTests[Inner].order)
    checkAll(s"$context.Order[Outer]", orderTests[Outer].order)
    checkAll(s"$context.Order[Interleaved[Int]]", orderTests[Interleaved[Int]].order)
    checkAll(s"$context.Order[Recursive]", orderTests[Recursive].order)
    checkAll(s"$context.Order[GenericAdt[Int]]", orderTests[GenericAdt[Int]].order)
    checkAll(s"$context.Order[EnumK0]", orderTests[EnumK0].order)
    checkAll(s"$context.Order is Serializable", SerializableTests.serializable(summonInline[Order[Interleaved[Int]]]))
  }

  {
    import auto.order.given
    testOrder("auto")
  }

  {
    import semiInstances.given
    testOrder("semiauto")
  }
}

object OrderSuite {
  import TestDefns._

  object semiInstances {
    given Order[Inner] = semiauto.order
    given Order[Outer] = semiauto.order
    given Order[Interleaved[Int]] = semiauto.order
    given Order[Recursive] = semiauto.order
    given Order[GenericAdt[Int]] = semiauto.order
    given Order[EnumK0] = semiauto.order
  }
}
