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

import cats.{Eq, Semigroup}
import cats.kernel.laws.discipline.{SemigroupTests, SerializableTests}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class SemigroupSuite extends KittensSuite:
  import SemigroupSuite.*
  import TestDefns.*

  inline def semigroupTests[A]: SemigroupTests[A] =
    SemigroupTests[A](summonInline)

  inline def testSemigroup(inline context: String): Unit =
    checkAll(s"$context.Semigroup[Foo]", semigroupTests[Foo].semigroup)
    checkAll(s"$context.Semigroup[Interleaved[Int]]", semigroupTests[Interleaved[Int]].semigroup)
    checkAll(s"$context.Semigroup[Box[Mul]]", semigroupTests[Box[Mul]].semigroup)
    // FIXME: Doesn't work
    //checkAll(s"$context.Semigroup[Recursive]", semigroupTests[Recursive].semigroup)
    checkAll(s"$context.Semigroup is Serializable", SerializableTests.serializable(summonInline[Semigroup[Foo]]))
    test(s"$context.Semigroup respects existing instances") {
      val box = summonInline[Semigroup[Box[Mul]]]
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)
    }

  locally {
    import auto.semigroup.given
    testSemigroup("auto")
  }

  locally {
    import semiInstances.given
    testSemigroup("semiauto")
  }

end SemigroupSuite

object SemigroupSuite:
  import TestDefns.*

  object semiInstances:
    given Semigroup[Foo] = semiauto.semigroup
    given Semigroup[Recursive] = semiauto.semigroup
    given Semigroup[Interleaved[Int]] = semiauto.semigroup
    given Semigroup[Box[Mul]] = semiauto.semigroup

  final case class Mul(value: Int)
  object Mul:
    given Eq[Mul] = Eq.fromUniversalEquals
    given Arbitrary[Mul] = Arbitrary(Arbitrary.arbitrary[Int].map(apply))
    given Semigroup[Mul] = (x, y) => Mul(x.value * y.value)

end SemigroupSuite
