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

import cats.derived.OrderSuite.Large
import cats.derived.TestDefns.{Foo, Outer}
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
    import cats.instances.string._ //so that Option instance could be derived
    // nasty local implicit Order instances that think that all things are equal
    implicit val orderInt: Order[Int] = Order.from((_, _) => 0)
    implicit def orderOption[A]: Order[Option[A]] = Order.from((_, _) => 0)

    forAll { (a: Foo, b: Foo) =>
      Order[Foo].compare(a, b) == 0
    }
  })

  test("semi derivation existing Order instances in scope are respected ")(check {
    import cats.instances.string._ //so that Option instance could be derived
    // nasty local implicit Order instances that think that all things are equal
    implicit val orderInt: Order[Int] = Order.from((_, _) => 0)
    implicit def orderOption[A]: Order[Option[A]] = Order.from((_, _) => 0)

    implicit val ordF: Order[Foo] = semi.order

    forAll { (a: Foo, b: Foo) =>
      Order[Foo].compare(a, b) == 0
    }
  })

  //compilation time
  {
    import auto.order._
    import cats.instances.all._
    semi.order[Large]
  }
}

object OrderSuite{
  case class Large(
                    bar1: String,
                    bar2: Int,
                    bar3: Boolean,
                    bar4: Large2,
                    bar5: List[String],
                    bar6: Boolean,
                    bar7: Double,
                    bar8: Long,
                    bar9: Char,
                    bar10: Float,
                    bar11: String,
                    bar12: String,
                    bar13: Boolean,
                    bar14: Option[String],
                    bar15: List[String],
                    bar16: Boolean,
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
                     bar6: Boolean,
                     bar7: Double,
                     bar8: Long,
                     bar9: Char,
                     bar10: Float,
                     bar11: String,
                     bar12: Int,
                     bar13: Boolean,
                     bar14: Option[String],
                     bar15: List[String],
                     bar16: Boolean,
                     bar17: Double,
                     bar18: Long,
                     bar19: Char,
                     bar20: Float,
                     bar21: String
                   )
}
