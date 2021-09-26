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

import cats.Eq
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import cats.kernel.laws.discipline.{CommutativeMonoidTests, SerializableTests}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class CommutativeMonoidSuite extends KittensSuite:
  import CommutativeMonoidSuite.*
  import TestDefns.*

  inline def commutativeMonoidTests[A]: CommutativeMonoidTests[A] =
    CommutativeMonoidTests[A](summonInline)

  inline def testCommutativeMonoid(inline context: String): Unit =
    checkAll(s"$context.CommutativeMonoid[Foo]", commutativeMonoidTests[CommutativeFoo].commutativeMonoid)
    // FIXME: Doesn't work
    //checkAll(s"$context.CommutativeMonoid[Recursive]", commutativeMonoidTests[Recursive].commutativeMonoid)
    checkAll(s"$context.CommutativeMonoid[Box[Mul]]", commutativeMonoidTests[Box[Mul]].commutativeMonoid)
    checkAll(
      s"$context.CommutativeMonoid is Serializable",
      SerializableTests.serializable(summonInline[CommutativeMonoid[CommutativeFoo]])
    )
    test(s"$context.CommutativeMonoid respects existing instances") {
      val box = summonInline[CommutativeMonoid[Box[Mul]]]
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
    }

  locally {
    import auto.commutativeMonoid.given
    testCommutativeMonoid("auto")
  }

  locally {
    import semiInstances.given
    testCommutativeMonoid("semiauto")
  }

end CommutativeMonoidSuite

object CommutativeMonoidSuite:
  import TestDefns.*

  object semiInstances:
    given CommutativeMonoid[CommutativeFoo] = semiauto.commutativeMonoid
    given CommutativeMonoid[Recursive] = semiauto.commutativeMonoid
    given CommutativeMonoid[Box[Mul]] = semiauto.commutativeMonoid

  final case class Mul(value: Int)
  object Mul:
    given Eq[Mul] = Eq.fromUniversalEquals
    given Arbitrary[Mul] = Arbitrary(Arbitrary.arbitrary[Int].map(apply))
    given CommutativeMonoid[Mul] with
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)

end CommutativeMonoidSuite
