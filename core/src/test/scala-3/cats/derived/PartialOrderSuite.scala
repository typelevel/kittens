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
import cats.kernel.laws.discipline.*
import scala.compiletime.*

class PartialOrderSuite extends KittensSuite:
  import PartialOrderSuite.*
  import ADTs.*

  inline def tests[A]: PartialOrderTests[A] =
    PartialOrderTests[A](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].partialOrder)
    checkAll(s"$instance[Inner]", tests[Inner].partialOrder)
    checkAll(s"$instance[Outer]", tests[Outer].partialOrder)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].partialOrder)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].partialOrder)
    checkAll(s"$instance[Recursive]", tests[Recursive].partialOrder)
    checkAll(s"$instance[Box[KeyValue]]", tests[Box[KeyValue]].partialOrder)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].partialOrder)
    checkAll(s"$instance[Singletons[Int]]", tests[Singletons[Int]].partialOrder)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[PartialOrder[Tree[Int]]]))
    test(s"$instance respects existing instances"):
      val boxKeyValue = summonInline[PartialOrder[Box[KeyValue]]]
      val x = Box(KeyValue("red", 1))
      val y = Box(KeyValue("red", 2))
      val z = Box(KeyValue("blue", 1))
      assert(boxKeyValue.partialCompare(x, y) < 0)
      assert(boxKeyValue.partialCompare(y, z).isNaN)

  locally:
    import auto.partialOrder.given
    validate("auto.partialOrder")

  locally:
    import semiInstances.given
    validate("semiauto.partialOrder")

  locally:
    import derivedInstances.*
    val instance = "derived.partialOrder"
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].partialOrder)
    checkAll(s"$instance[Inner]", tests[Inner].partialOrder)
    checkAll(s"$instance[Outer]", tests[Outer].partialOrder)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].partialOrder)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].partialOrder)
    checkAll(s"$instance[Recursive]", tests[Recursive].partialOrder)
    checkAll(s"$instance[BoxKV]", tests[BoxKV].partialOrder)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].partialOrder)
    checkAll(s"$instance[Singletons[Int]]", tests[Singletons[Int]].partialOrder)
    checkAll(s"$instance is Serialiable", SerializableTests.serializable(PartialOrder[Tree[Int]]))

end PartialOrderSuite

object PartialOrderSuite:
  import ADTs.*

  object semiInstances:
    given PartialOrder[IList[Int]] = semiauto.partialOrder
    given PartialOrder[Inner] = semiauto.partialOrder
    given PartialOrder[Outer] = semiauto.partialOrder
    given PartialOrder[Interleaved[Int]] = semiauto.partialOrder
    given PartialOrder[Tree[Int]] = semiauto.partialOrder
    given PartialOrder[Recursive] = semiauto.partialOrder
    given PartialOrder[Box[KeyValue]] = semiauto.partialOrder
    given PartialOrder[EnumK0] = semiauto.partialOrder
    given PartialOrder[Singletons[Int]] = semiauto.partialOrder

  object derivedInstances:
    case class IList[A](x: ADTs.IList[A]) derives PartialOrder
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives PartialOrder
    case class Tree[A](x: ADTs.Tree[A]) derives PartialOrder
    case class Inner(x: ADTs.Inner) derives PartialOrder
    case class Outer(x: ADTs.Outer) derives PartialOrder
    case class Recursive(x: ADTs.Recursive) derives PartialOrder
    case class EnumK0(x: ADTs.EnumK0) derives PartialOrder
    case class Singletons[A](x: ADTs.Singletons[A]) derives PartialOrder
    case class BoxKV(x: Box[KeyValue]) derives PartialOrder

  final case class KeyValue(key: String, value: Int)
  object KeyValue:
    given PartialOrder[KeyValue] =
      PartialOrder.from((x, y) => if x.key == y.key then x.value.toDouble - y.value.toDouble else Double.NaN)

end PartialOrderSuite
