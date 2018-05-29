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

import cats.PartialOrder
import cats.derived.PartialOrderSuite.Large
import cats.kernel.laws.discipline._
import cats.derived.TestDefns.Foo
import cats.derived.TestDefns.{IList, Outer}
import org.scalacheck.Prop.forAll


class PartialOrderSuite extends KittensSuite {
  {
    import auto.partialOrder._
    import cats.instances.int._
    checkAll("IList[Int]", PartialOrderTests[IList[Int]].partialOrder)
  }
  {

    import auto.partialOrder._
    import cats.instances.all._

    checkAll("Outer", PartialOrderTests[Outer].partialOrder)
  }


  test("IList PartialOrder consistent with universal equality")(check {

    import auto.partialOrder._
    import cats.instances.int._

    forAll { (a: IList[Int], b: IList[Int]) =>
      PartialOrder[IList[Int]].eqv(a, b) == (a == b)
    }
  })


  test("existing PartialOrder instances in scope are respected")(check {

    import auto.partialOrder._
    import cats.instances.double._

    // nasty local implicit PartialOrder instances that think that all things are equal
    implicit def partialOrderInt: PartialOrder[Int] = PartialOrder.from((_, _) => 0)
    implicit def partialOrderOption[A]: PartialOrder[Option[A]] = PartialOrder.from((_, _) => 0)

    forAll { (a: Foo, b: Foo) =>
      PartialOrder[Foo].partialCompare(a, b) == 0
    }
  })

  test("semi derivation existing PartialOrder instances in scope are respected ")(check {

    import cats.instances.double._

    // nasty local implicit PartialOrder instances that think that all things are equal
    implicit def partialOrderInt: PartialOrder[Int] = PartialOrder.from((_, _) => 0)
    implicit def partialOrderOption[A]: PartialOrder[Option[A]] = PartialOrder.from((_, _) => 0)

    implicit val eqF: PartialOrder[Foo] = semi.partialOrder

    forAll { (a: Foo, b: Foo) =>
      PartialOrder[Foo].partialCompare(a, b) == 0
    }
  })

  //compilation time
  {
    import auto.partialOrder._
    import cats.instances.all._
    semi.partialOrder[Large]
  }
}

object PartialOrderSuite{
  case class Large(
                    bar1: String,
                    bar2: Int,
                    bar3: Boolean,
                    bar4: Large2,
                    bar5: List[String],
                    bar6: Set[Boolean],
                    bar7: Double,
                    bar8: Long,
                    bar9: Char,
                    bar10: Float,
                    bar11: String,
                    bar12: String,
                    bar13: Boolean,
                    bar14: Option[String],
                    bar15: List[String],
                    bar16: Set[Boolean],
                    bar17: Double,
                    bar18: Long,
                    bar19: Char,
                    bar20: Float
                  )

  case class Large2(
                     bar1: String,
                     bar2: Int,
                     bar3: Boolean,
                     bar4: Option[String],
                     bar5: List[String],
                     bar6: Set[Boolean],
                     bar7: Double,
                     bar8: Long,
                     bar9: Char,
                     bar10: Float,
                     bar11: String,
                     bar12: Int,
                     bar13: Boolean,
                     bar14: Option[String],
                     bar15: List[String],
                     bar16: Set[Boolean],
                     bar17: Double,
                     bar18: Long,
                     bar19: Char,
                     bar20: Float,
                     bar21: String
                   )
}
