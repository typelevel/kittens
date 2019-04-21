/*
 * Copyright (c) 2016 Miles Sabin
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

import cats.{Eq, Monoid}
import cats.instances.all._
import cats.kernel.laws.discipline._
import org.scalacheck.Arbitrary

class MonoidSuite extends KittensSuite {
  import MonoidSuite._
  import TestDefns._

  def testMonoid(context: String)(
    implicit foo: Monoid[Foo],
    recursive: Monoid[Recursive],
    interleaved: Monoid[Interleaved[Int]],
    box: Monoid[Box[Mul]]
  ): Unit = {
    checkAll(s"$context.Monoid[Foo]", MonoidTests[Foo].monoid)
    checkAll(s"$context.Monoid[Recursive]", MonoidTests[Recursive].monoid)
    checkAll(s"$context.Monoid[Interleaved[Int]]", MonoidTests[Interleaved[Int]].monoid)
    checkAll(s"$context.Monoid[Box[Mul]]", MonoidTests[Box[Mul]].monoid)

    test(s"$context.Monoid respects existing instances") {
      assert(box.empty.content.value == 1)
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)
    }
  }

  {
    import auto.monoid._
    testMonoid("auto")
  }

  {
    import cached.monoid._
    testMonoid("cached")
  }

  {
    implicit val foo: Monoid[Foo] = semi.monoid
    implicit lazy val recursive: Monoid[Recursive] = semi.monoid
    implicit val interleaved: Monoid[Interleaved[Int]] = semi.monoid
    implicit val box: Monoid[Box[Mul]] = semi.monoid
    testMonoid("semi")
  }
}

object MonoidSuite {

  final case class Mul(value: Int)
  object Mul {

    implicit val eqv: Eq[Mul] =
      Eq.fromUniversalEquals

    implicit val arbitrary: Arbitrary[Mul] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val monoid: Monoid[Mul] = new Monoid[Mul] {
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)
    }
  }
}
