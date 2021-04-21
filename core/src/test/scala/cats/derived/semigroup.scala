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
import cats.kernel.laws.discipline.{SemigroupTests, SerializableTests}
import org.scalacheck.Arbitrary

class SemigroupSuite extends KittensSuite {
  import SemigroupSuite._
  import TestDefns._
  import TestEqInstances._

  def testSemigroup(context: String)(implicit
      foo: Semigroup[Foo],
      recursive: Semigroup[Recursive],
      interleaved: Semigroup[Interleaved[Int]],
      box: Semigroup[Box[Mul]]
  ): Unit = {
    checkAll(s"$context.Semigroup[Foo]", SemigroupTests[Foo].semigroup)
    checkAll(s"$context.Semigroup[Recursive]", SemigroupTests[Recursive].semigroup)
    checkAll(s"$context.Semigroup[Interleaved[Int]]", SemigroupTests[Interleaved[Int]].semigroup)
    checkAll(s"$context.Semigroup[Box[Mul]]", SemigroupTests[Box[Mul]].semigroup)
    checkAll(s"$context.Semigroup is Serializable", SerializableTests.serializable(Semigroup[Interleaved[Int]]))

    test(s"$context.Semigroup respects existing instances") {
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)
    }
  }

  {
    import auto.semigroup._
    testSemigroup("auto")
  }

  {
    import cached.semigroup._
    testSemigroup("cached")
  }

  {
    import semiInstances._
    testSemigroup("semiauto")
  }
}

object SemigroupSuite {
  import TestDefns._

  object semiInstances {
    implicit val foo: Semigroup[Foo] = semiauto.semigroup
    implicit lazy val recursive: Semigroup[Recursive] = semiauto.semigroup
    implicit val interleaved: Semigroup[Interleaved[Int]] = semiauto.semigroup
    implicit val box: Semigroup[Box[Mul]] = semiauto.semigroup
  }

  final case class Mul(value: Int)
  object Mul {

    implicit val eqv: Eq[Mul] =
      Eq.fromUniversalEquals

    implicit val arbitrary: Arbitrary[Mul] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val semigroup: Semigroup[Mul] =
      Semigroup.instance((x, y) => Mul(x.value * y.value))
  }
}
