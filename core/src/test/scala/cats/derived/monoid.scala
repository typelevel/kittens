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
import cats.kernel.laws.discipline.{MonoidTests, SerializableTests}
import org.scalacheck.Arbitrary

class MonoidSuite extends KittensSuite {
  import MonoidSuite._
  import TestDefns._
  import TestEqInstances._

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
    checkAll(s"$context.Monoid is Serializable", SerializableTests.serializable(Monoid[Interleaved[Int]]))

    test(s"$context.Monoid respects existing instances") {
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
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
    import semiInstances._
    testMonoid("semiauto")
  }
}

object MonoidSuite {
  import TestDefns._

  object semiInstances {
    implicit val foo: Monoid[Foo] = semiauto.monoid
    implicit lazy val recursive: Monoid[Recursive] = semiauto.monoid
    implicit val interleaved: Monoid[Interleaved[Int]] = semiauto.monoid
    implicit val box: Monoid[Box[Mul]] = semiauto.monoid
  }

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
