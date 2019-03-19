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
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary
import cats.derived.EqSuite.Foo
import cats.derived.TestDefns.{IList, Inner, Large, Outer}


class EqSuite extends KittensSuite {


  {
    import auto.eq._

    import cats.instances.int._
    checkAll("IList[Int]", EqTests[IList[Int]].eqv)
  }
  {

    import auto.eq._
    import cats.instances.all._
    checkAll("Outer", EqTests[Outer].eqv)
  }


  test("IList Eq consistent with universal equality")(check {

    import auto.eq._
    import cats.instances.int._

    forAll { (a: IList[Int], b: IList[Int]) =>
      Eq[IList[Int]].eqv(a, b) == (a == b)
    }
  })


  test("existing Eq instances in scope are respected for auto derivation")(check {

    import auto.eq._

    // nasty local implicit Eq instances that think that all things are equal
    implicit def eqInner: Eq[Inner] = Eq.instance((_, _) => true)

    forAll { (a: Outer, b: Outer) =>
      Eq[Outer].eqv(a, b)
    }
  })

  test("existing Eq instances in scope are respected for semi derivation")(check {


    // nasty local implicit Eq instances that think that all things are equal
    implicit def eqInner: Eq[Inner] = Eq.instance((_, _) => true)

    implicit val eqOuter: Eq[Outer] = semi.eq

    forAll { (a: Outer, b: Outer) =>
      Eq[Outer].eqv(a, b)
    }
  })

  //compilation time
  {
    import auto.eq._
    import cats.instances.all._
    semi.eq[Large]
  }

  test("derives an instance for Interleaved[T]") {
    import cats.instances.all._
    semi.eq[TestDefns.Interleaved[Int]]
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
