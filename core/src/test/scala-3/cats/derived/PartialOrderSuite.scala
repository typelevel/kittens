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

import cats.kernel.laws.discipline.{PartialOrderTests, SerializableTests}
import org.scalacheck.{Arbitrary, Cogen}
import scala.compiletime.*

class PartialOrderSuite extends KittensSuite:
  import PartialOrderSuite.*
  import TestDefns.*

  inline def partialOrderTests[A]: PartialOrderTests[A] =
    PartialOrderTests[A](summonInline)

  inline def testPartialOrder(context: String): Unit =
    checkAll(s"$context.PartialOrder[IList[Int]]", partialOrderTests[IList[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Inner]", partialOrderTests[Inner].partialOrder)
    checkAll(s"$context.PartialOrder[Outer]", partialOrderTests[Outer].partialOrder)
    checkAll(s"$context.PartialOrder[Interleaved[Int]]", partialOrderTests[Interleaved[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Tree[Int]]", partialOrderTests[Tree[Int]].partialOrder)
    checkAll(s"$context.PartialOrder[Recursive]", partialOrderTests[Recursive].partialOrder)
    checkAll(s"$context.PartialOrder[Box[KeyValue]]", partialOrderTests[Box[KeyValue]].partialOrder)
    checkAll(s"$context.PartialOrder[EnumK0]", partialOrderTests[EnumK0].partialOrder)
    checkAll(
      s"$context.PartialOrder is Serialiable",
      SerializableTests.serializable(summonInline[PartialOrder[Tree[Int]]])
    )

    test(s"$context.PartialOrder respects existing instances") {
      val boxKeyValue = summonInline[PartialOrder[Box[KeyValue]]]
      val x = Box(KeyValue("red", 1))
      val y = Box(KeyValue("red", 2))
      val z = Box(KeyValue("blue", 1))
      assert(boxKeyValue.partialCompare(x, y) < 0)
      assert(boxKeyValue.partialCompare(y, z).isNaN)
    }

  locally {
    import auto.partialOrder.given
    testPartialOrder("auto")
  }

  locally {
    import semiInstances.given
    testPartialOrder("semiauto")
  }

end PartialOrderSuite

object PartialOrderSuite:
  import TestDefns.*

  object semiInstances:
    given PartialOrder[IList[Int]] = semiauto.partialOrder
    given PartialOrder[Inner] = semiauto.partialOrder
    given PartialOrder[Outer] = semiauto.partialOrder
    given PartialOrder[Interleaved[Int]] = semiauto.partialOrder
    given PartialOrder[Tree[Int]] = semiauto.partialOrder
    given PartialOrder[Recursive] = semiauto.partialOrder
    given PartialOrder[Box[KeyValue]] = semiauto.partialOrder
    given PartialOrder[EnumK0] = semiauto.partialOrder

  final case class KeyValue(key: String, value: Int)
  object KeyValue extends ((String, Int) => KeyValue):
    given Arbitrary[KeyValue] = Arbitrary(Arbitrary.arbitrary[(String, Int)].map(tupled))

    given Cogen[KeyValue] = Cogen[(String, Int)].contramap(kv => kv.key -> kv.value)

    given PartialOrder[KeyValue] =
      PartialOrder.from((x, y) => if (x.key == y.key) x.value.toDouble - y.value.toDouble else Double.NaN)

end PartialOrderSuite
