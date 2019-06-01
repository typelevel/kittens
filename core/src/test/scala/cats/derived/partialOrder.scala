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

import cats.instances.all._
import cats.kernel.laws.discipline._
import org.scalacheck.{Arbitrary, Cogen}

class PartialOrderSuite extends KittensSuite {
  import PartialOrderSuite._
  import TestDefns._

  def testPartialOrder(context: String)(
    implicit iList: PartialOrder[IList[Int]],
    inner: PartialOrder[Inner],
    outer: PartialOrder[Outer],
    interleaved: PartialOrder[Interleaved[Int]],
    tree: PartialOrder[Tree[Int]],
    recursive: PartialOrder[Recursive],
    boxKeyValue: PartialOrder[Box[KeyValue]]
  ): Unit = {
    checkAll(s"$context.PartialOrder[IList[Int]]", PartialOrderTests[IList[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Inner]", PartialOrderTests[Inner].partialOrder)
    checkAll(s"$context.PartialOrder[Outer]", PartialOrderTests[Outer].partialOrder)
    checkAll(s"$context.PartialOrder[Interleaved[Int]]", PartialOrderTests[Interleaved[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Tree[Int]]", PartialOrderTests[Tree[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Recursive]", PartialOrderTests[Recursive].partialOrder)
    checkAll(s"$context.PartialOrder[Box[KeyValue]]", PartialOrderTests[Box[KeyValue]].partialOrder)

    test(s"$context.PartialOrder respects existing instances") {
      val x = Box(KeyValue("red", 1))
      val y = Box(KeyValue("red", 2))
      val z = Box(KeyValue("blue", 1))
      assert(boxKeyValue.partialCompare(x, y) < 0)
      assert(boxKeyValue.partialCompare(y, z).isNaN)
    }
  }

  {
    import auto.partialOrder._
    testPartialOrder("auto")
  }

  {
    import cached.partialOrder._
    testPartialOrder("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val iList: PartialOrder[IList[Int]] = semi.partialOrder
    implicit val inner: PartialOrder[Inner] = semi.partialOrder
    implicit val outer: PartialOrder[Outer] = semi.partialOrder
    implicit val interleaved: PartialOrder[Interleaved[Int]] = semi.partialOrder
    implicit val tree: PartialOrder[Tree[Int]] = semi.partialOrder
    implicit val recursive: PartialOrder[Recursive] = semi.partialOrder
    implicit val boxKeyValue: PartialOrder[Box[KeyValue]] = semi.partialOrder
    def run(): Unit = testPartialOrder("semi")
  }
}

object PartialOrderSuite {

  final case class KeyValue(key: String, value: Int)
  object KeyValue extends ((String, Int) => KeyValue) {
    implicit val arbitrary: Arbitrary[KeyValue] = Arbitrary(Arbitrary.arbitrary[(String, Int)].map(tupled))
    implicit val cogen: Cogen[KeyValue] = Cogen[(String, Int)].contramap(unapply(_).get)

    implicit val partialOrder: PartialOrder[KeyValue] =
      PartialOrder.from((x, y) => if (x.key == y.key) x.value.toDouble - y.value.toDouble else Double.NaN)
  }
}
