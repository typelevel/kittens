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

import cats.kernel.CommutativeSemigroup
import cats.kernel.laws.discipline.{CommutativeSemigroupTests, SerializableTests}
import org.scalacheck.Arbitrary

class CommutativeSemigroupSuite extends KittensSuite {
  import CommutativeSemigroupSuite._
  import TestDefns._
  import TestEqInstances._

  def testCommutativeSemigroup(context: String)(
    implicit foo: CommutativeSemigroup[CommutativeFoo],
    recursive: CommutativeSemigroup[Recursive],
    box: CommutativeSemigroup[Box[Mul]]
  ): Unit = {
    checkAll(s"$context.CommutativeSemigroup[CommutativeFoo]", CommutativeSemigroupTests[CommutativeFoo].commutativeSemigroup)
    checkAll(s"$context.CommutativeSemigroup[Recursive]", CommutativeSemigroupTests[Recursive].commutativeSemigroup)
    checkAll(s"$context.CommutativeSemigroup[Box[Mul]]", CommutativeSemigroupTests[Box[Mul]].commutativeSemigroup)
    checkAll(s"$context.CommutativeSemigroup is Serializable", SerializableTests.serializable(CommutativeSemigroup[CommutativeFoo]))

    test(s"$context.CommutativeSemigroup respects existing instances") {
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)
    }
  }

  {
    import auto.commutativeSemigroup._
    testCommutativeSemigroup("auto")
  }

  {
    import cached.commutativeSemigroup._
    testCommutativeSemigroup("cached")
  }

  {
    implicit val foo: CommutativeSemigroup[CommutativeFoo] = semiauto.commutativeSemigroup
    implicit lazy val recursive: CommutativeSemigroup[Recursive] = semiauto.commutativeSemigroup
    implicit val box: CommutativeSemigroup[Box[Mul]] = semiauto.commutativeSemigroup
    testCommutativeSemigroup("semi")
  }
}

object CommutativeSemigroupSuite {

  // can be removed once kittens depends on a version of cats that includes https://github.com/typelevel/cats/pull/2834
  implicit def commutativeSemigroupOption[A](implicit sa: CommutativeSemigroup[A]): CommutativeSemigroup[Option[A]] = new CommutativeSemigroup[Option[A]] {
    def combine(x: Option[A], y: Option[A]): Option[A] = cats.instances.option.catsKernelStdMonoidForOption(sa).combine(x, y)
  }

  final case class Mul(value: Int)
  object Mul {

    implicit val eqv: Eq[Mul] =
      Eq.fromUniversalEquals

    implicit val arbitrary: Arbitrary[Mul] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val commutativeSemigroup: CommutativeSemigroup[Mul] =
      CommutativeSemigroup.instance((x, y) => Mul(x.value * y.value))
  }
}
