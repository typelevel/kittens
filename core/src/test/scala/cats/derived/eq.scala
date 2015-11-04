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

import cats.Eq
import algebra.laws.OrderLaws
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import TestDefns._

class EqTests extends CatsSuite {
  import cats.derived.eq._
  import EqTests._

  // this results in:
  // java.lang.NoClassDefFoundError: org/scalatest/FunSuiteRegistration
  //checkAll("IList[Int]", OrderLaws[IList[Int]].eqv)

  test("IList Eq consistent with universal equality")(check {
    forAll { (a: IList[Int], b: IList[Int]) =>
      Eq[IList[Int]].eqv(a, b) == (a == b)
    }
  })

  test("existing Eq instances in scope are respected")(check {
    // nasty local implicit Eq instances that think that all things are equal
    implicit val eqInt: Eq[Int] = Eq.instance((_, _) => true)
    implicit def eqOption[A]: Eq[Option[A]] = Eq.instance((_, _) => true)

    forAll { (a: Foo, b: Foo) =>
      Eq[Foo].eqv(a, b)
    }
  })
}

object EqTests {
  final case class Foo(i: Int, b: Option[Boolean])

  implicit val arbFoo: Arbitrary[Foo] = Arbitrary(
    for {
      i <- arbitrary[Int]
      b <- arbitrary[Option[Boolean]]
    } yield Foo(i, b))
}
