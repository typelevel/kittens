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

package cats
package derived
import cats.laws.discipline.{SemigroupKTests, SerializableTests}
import org.scalacheck.Arbitrary

class SemigroupKSuite extends KittensSuite {
  import SemigroupKSuite._
  import TestDefns._
  import TestEqInstances._

  def testSemigroupK(context: String)(
    implicit complexProduct: SemigroupK[ComplexProduct],
    caseClassWOption: SemigroupK[CaseClassWOption],
    boxMul: SemigroupK[BoxMul]
  ): Unit = {
    checkAll(s"$context.SemigroupK[ComplexProduct]", SemigroupKTests[ComplexProduct].semigroupK[Char])
    checkAll(s"$context.SemigroupK[CaseClassWOption]", SemigroupKTests[CaseClassWOption].semigroupK[Char])
    checkAll(s"$context.SemigroupK[BoxMul]", SemigroupKTests[BoxMul].semigroupK[Char])
    checkAll(s"$context.SemigroupK is Serializable", SerializableTests.serializable(SemigroupK[ComplexProduct]))

    test(s"$context.SemigroupK respects existing instances") {
      assert(boxMul.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }
  }

  {
    import auto.semigroupK._
    testSemigroupK("auto")
  }

  {
    import cached.semigroupK._
    testSemigroupK("cached")
  }

  {
    import semiInstances._
    testSemigroupK("semiauto")
  }
}

object SemigroupKSuite {
  import TestDefns._

  type BoxMul[A] = Box[Mul[A]]

  object semiInstances {
    implicit val complexProduct: SemigroupK[ComplexProduct] = semiauto.semigroupK
    implicit val caseClassWOption: SemigroupK[CaseClassWOption] = semiauto.semigroupK
    implicit val boxMul: SemigroupK[BoxMul] = semiauto.semigroupK
  }

  final case class Mul[T](value: Int)
  object Mul {

    implicit def eqv[T]: Eq[Mul[T]] = Eq.by(_.value)

    implicit def arbitrary[T]: Arbitrary[Mul[T]] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val semigroupK: SemigroupK[Mul] = new SemigroupK[Mul] {
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)
    }
  }
}
