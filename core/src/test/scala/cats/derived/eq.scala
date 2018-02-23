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

import cats.Eq
import cats.kernel.laws.discipline._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary
import cats.derived.EqSuite.Foo
import cats.derived.TestDefns.{IList, Large, Outer}
import auto.eq._


class EqSuite extends KittensSuite {


  {
    import cats.instances.int._
    checkAll("IList[Int]", EqTests[IList[Int]].eqv)
  }
  {
    import cats.instances.all._
    checkAll("Outer", EqTests[Outer].eqv)
  }


  test("IList Eq consistent with universal equality")(check {
    import cats.instances.int._

    forAll { (a: IList[Int], b: IList[Int]) =>
      Eq[IList[Int]].eqv(a, b) == (a == b)
    }
  })

  test("existing Eq instances in scope are respected")(check {
    import cats.instances.boolean._

    // nasty local implicit Eq instances that think that all things are equal
    implicit def eqInt: Eq[Int] = Eq.instance((_, _) => true)
    implicit def eqOption[A]: Eq[Option[A]] = Eq.instance((_, _) => true)

    forAll { (a: Foo, b: Foo) =>
      Eq[Foo].eqv(a, b)
    }
  })

  //compilation time
  {
    import cats.instances.all._
    semi.eq[Large]
  }
}

object EqSuite {

  //redefine this because the one in TestDefn comes with a Eq instance and there is no way to shadow it.
  final case class Foo(i: Int, b: Option[String])
  implicit val arbFoo: Arbitrary[Foo] =
    Arbitrary(for {
      i <- arbitrary[Int]
      b <- arbitrary[Option[String]]
    } yield Foo(i, b))

}
