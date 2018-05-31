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

import cats.kernel.laws.discipline._
import cats.derived.TestDefns.{Foo, IList, Inner, Large4, Outer}
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

  test("existing PartialOrder instances in scope are respected for auto derivation")(check {

    import auto.partialOrder._

    // nasty local implicit PartialOrder instances that think that all things are equal
    implicit def partialOrderInner: PartialOrder[Inner] = PartialOrder.from((_, _) => 0)

    forAll { (a: Outer, b: Outer) =>
      PartialOrder[Outer].partialCompare(a, b) == 0
    }
  })

  test("existing PartialOrder instances in scope are respected for semi derivation")(check {

    // nasty local implicit PartialOrder instances that think that all things are equal
    implicit def partialOrderInner: PartialOrder[Inner] = PartialOrder.from((_, _) => 0)

    implicit val ordF: PartialOrder[Outer] = semi.partialOrder

    forAll { (a: Outer, b: Outer) =>
      PartialOrder[Outer].partialCompare(a, b) == 0
    }
  })
  
  //compilation time
  {
    import auto.partialOrder._
    import cats.instances.all._

    semi.partialOrder[Large4]
  }
}
