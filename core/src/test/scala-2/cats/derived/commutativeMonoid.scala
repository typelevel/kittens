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
import cats.kernel.CommutativeMonoid
import cats.kernel.laws.discipline.{CommutativeMonoidTests, SerializableTests}
import org.scalacheck.Arbitrary

class CommutativeMonoidSuite extends KittensSuite {
  import CommutativeMonoidSuite._
  import TestDefns._
  import TestEqInstances._

  def testCommutativeMonoid(context: String)(implicit
      commutativeFoo: CommutativeMonoid[CommutativeFoo],
      recursive: CommutativeMonoid[Recursive],
      box: CommutativeMonoid[Box[Mul]]
  ): Unit = {
    checkAll(s"$context.CommutativeMonoid[Foo]", CommutativeMonoidTests[CommutativeFoo].commutativeMonoid)
    checkAll(s"$context.CommutativeMonoid[Recursive]", CommutativeMonoidTests[Recursive].commutativeMonoid)
    checkAll(s"$context.CommutativeMonoid[Box[Mul]]", CommutativeMonoidTests[Box[Mul]].commutativeMonoid)
    checkAll(
      s"$context.CommutativeMonoid is Serializable",
      SerializableTests.serializable(CommutativeMonoid[CommutativeFoo])
    )

    test(s"$context.CommutativeMonoid respects existing instances") {
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
    }
  }

  {
    import auto.commutativeMonoid._
    testCommutativeMonoid("auto")
  }

  {
    import cached.commutativeMonoid._
    testCommutativeMonoid("cached")
  }

  {
    implicit val foo: CommutativeMonoid[CommutativeFoo] = semiauto.commutativeMonoid
    implicit lazy val recursive: CommutativeMonoid[Recursive] = semiauto.commutativeMonoid
    implicit val box: CommutativeMonoid[Box[Mul]] = semiauto.commutativeMonoid
    testCommutativeMonoid("semi")
  }
}

object CommutativeMonoidSuite {

  final case class Mul(value: Int)
  object Mul {

    implicit val eqv: Eq[Mul] =
      Eq.fromUniversalEquals

    implicit val arbitrary: Arbitrary[Mul] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val monoidMul: CommutativeMonoid[Mul] = new CommutativeMonoid[Mul] {
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)
    }
  }
}
