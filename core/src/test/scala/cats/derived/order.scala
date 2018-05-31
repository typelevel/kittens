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

import cats.derived.TestDefns.{Foo, Inner, Large4, Outer}
import cats.kernel.laws.discipline._
import org.scalacheck.Prop.forAll


class OrderSuite extends KittensSuite {
  {
    import auto.order._
    import cats.instances.all._

    checkAll("Foo", OrderTests[Foo].order)
  }
  {

    import auto.order._
    import cats.instances.all._

    checkAll("Outer", OrderTests[Outer].order)
  }


  test("Foo Order consistent with universal equality")(check {

    import auto.order._
    import cats.instances.all._

    forAll { (a: Foo, b: Foo) =>
      Order[Foo].eqv(a, b) == (a == b)
    }
  })


  test("existing Order instances in scope are respected")(check {

    import auto.order._

    // nasty local implicit Order instances that think that all things are equal
    implicit def orderInner: Order[Inner] = Order.from((_, _) => 0)

    forAll { (a: Outer, b: Outer) =>
      Order[Outer].compare(a, b) == 0
    }
  })

  test("semi derivation existing Order instances in scope are respected ")(check {

    // nasty local implicit Order instances that think that all things are equal
    implicit def orderInner: Order[Inner] = Order.from((_, _) => 0)

    implicit val ordF: Order[Outer] = semi.order

    forAll { (a: Outer, b: Outer) =>
      Order[Outer].compare(a, b) == 0
    }
  })

  //compilation time
  {
    import auto.order._
    import cats.instances.all._
    semi.order[Large4]
  }
}
