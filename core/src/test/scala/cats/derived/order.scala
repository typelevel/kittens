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

package cats
package derived

import cats.kernel.laws.discipline.{OrderTests, SerializableTests}
import cats.instances.all._

class OrderSuite extends KittensSuite {
  import TestDefns._

  def testOrder(context: String)(
    implicit inner: Order[Inner],
    outer: Order[Outer],
    interleaved: Order[Interleaved[Int]],
    recursive: Order[Recursive],
    genericAdt: Order[GenericAdt[Int]]
  ): Unit = {
    checkAll(s"$context.Order[Inner]", OrderTests[Inner].order)
    checkAll(s"$context.Order[Outer]", OrderTests[Outer].order)
    checkAll(s"$context.Order[Interleaved[Int]]", OrderTests[Interleaved[Int]].order)
    checkAll(s"$context.Order[Recursive]", OrderTests[Recursive].order)
    checkAll(s"$context.Order[GenericAdt[Int]]", OrderTests[GenericAdt[Int]].order)
    checkAll(s"$context.Order is Serializable", SerializableTests.serializable(Order[Interleaved[Int]]))
  }

  {
    import auto.order._
    testOrder("auto")
  }

  {
    import cached.order._
    testOrder("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val inner: Order[Inner] = semi.order
    implicit val outer: Order[Outer] = semi.order
    implicit val interleaved: Order[Interleaved[Int]] = semi.order
    implicit val recursive: Order[Recursive] = semi.order
    implicit val genericAdt: Order[GenericAdt[Int]] = semi.order
    def run(): Unit = testOrder("semi")
  }
}
