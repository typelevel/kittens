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
import cats.kernel.CommutativeSemigroup
import cats.kernel.laws.discipline.{CommutativeSemigroupTests, SerializableTests}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class CommutativeSemigroupSuite extends KittensSuite:
  import CommutativeSemigroupSuite.*
  import TestDefns.*

  inline def commutativeSemigroupTests[A]: CommutativeSemigroupTests[A] =
    CommutativeSemigroupTests[A](summonInline)

  inline def testCommutativeSemigroup(inline context: String): Unit =
    checkAll(
      s"$context.CommutativeSemigroup[CommutativeFoo]",
      commutativeSemigroupTests[CommutativeFoo].commutativeSemigroup
    )
    // FIXME: Doesn't work
    //checkAll(s"$context.CommutativeSemigroup[Recursive]", commutativeSemigroupTests[Recursive].commutativeSemigroup)
    checkAll(s"$context.CommutativeSemigroup[Box[Mul]]", commutativeSemigroupTests[Box[Mul]].commutativeSemigroup)
    checkAll(
      s"$context.CommutativeSemigroup is Serializable",
      SerializableTests.serializable(summonInline[CommutativeSemigroup[CommutativeFoo]])
    )
    test(s"$context.CommutativeSemigroup respects existing instances") {
      val box = summonInline[CommutativeSemigroup[Box[Mul]]]
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)
    }

  locally {
    import auto.commutativeSemigroup.given
    testCommutativeSemigroup("auto")
  }

  locally {
    import semiInstances.given
    testCommutativeSemigroup("semiauto")
  }

end CommutativeSemigroupSuite

object CommutativeSemigroupSuite:
  import TestDefns.*

  object semiInstances:
    given CommutativeSemigroup[CommutativeFoo] = semiauto.commutativeSemigroup
    given CommutativeSemigroup[Recursive] = semiauto.commutativeSemigroup
    given CommutativeSemigroup[Box[Mul]] = semiauto.commutativeSemigroup

  final case class Mul(value: Int)
  object Mul:
    given Eq[Mul] = Eq.fromUniversalEquals
    given Arbitrary[Mul] = Arbitrary(Arbitrary.arbitrary[Int].map(apply))
    given CommutativeSemigroup[Mul] = (x, y) => Mul(x.value * y.value)

end CommutativeSemigroupSuite
