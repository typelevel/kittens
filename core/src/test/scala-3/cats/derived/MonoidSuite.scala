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
import cats.kernel.laws.discipline.{MonoidTests, SerializableTests}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class MonoidSuite extends KittensSuite:
  import MonoidSuite.*
  import TestDefns.*

  inline def monoidTests[A]: MonoidTests[A] =
    MonoidTests[A](summonInline)

  inline def testMonoid(inline context: String): Unit =
    checkAll(s"$context.Monoid[Foo]", monoidTests[Foo].monoid)
    checkAll(s"$context.Monoid[Interleaved[Int]]", monoidTests[Interleaved[Int]].monoid)
    checkAll(s"$context.Monoid[Box[Mul]]", monoidTests[Box[Mul]].monoid)
    checkAll(s"$context.Monoid[Recursive]", monoidTests[Recursive].monoid)
    checkAll(s"$context.Monoid is Serializable", SerializableTests.serializable(summonInline[Monoid[Foo]]))
    test(s"$context.Monoid respects existing instances") {
      val box = summonInline[Monoid[Box[Mul]]]
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
    }

  locally {
    import auto.monoid.given
    testMonoid("auto")
  }

  locally {
    import semiInstances.given
    testMonoid("semiauto")
  }

end MonoidSuite

object MonoidSuite:
  import TestDefns.*

  object semiInstances:
    given Monoid[Foo] = semiauto.monoid
    given Monoid[Recursive] = semiauto.monoid
    given Monoid[Interleaved[Int]] = semiauto.monoid
    given Monoid[Box[Mul]] = semiauto.monoid

  final case class Mul(value: Int)
  object Mul:
    given Eq[Mul] = Eq.fromUniversalEquals
    given Arbitrary[Mul] = Arbitrary(Arbitrary.arbitrary[Int].map(apply))
    given Monoid[Mul] with
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)

end MonoidSuite
