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

import cats.Semigroup
import cats.kernel.laws.discipline.*
import org.scalacheck.Arbitrary

import scala.compiletime.*

class SemigroupSuite extends KittensSuite:
  import SemigroupSuite.*
  import ADTs.*

  inline def tests[A]: SemigroupTests[A] =
    SemigroupTests[A](using summonInline)

  inline def validate(inline instances: String): Unit =
    checkAll(s"$instances[Foo]", tests[Foo].semigroup)
    checkAll(s"$instances[Interleaved[Int]]", tests[Interleaved[Int]].semigroup)
    checkAll(s"$instances[Box[Mul]]", tests[Box[Mul]].semigroup)
    checkAll(s"$instances[Recursive]", tests[Recursive].semigroup)
    checkAll(s"$instances is Serializable", SerializableTests.serializable(summonInline[Semigroup[Foo]]))
    test(s"$instances respects existing instances"):
      val box = summonInline[Semigroup[Box[Mul]]]
      assert(box.combine(Box(Mul(5)), Box(Mul(5))).content.value == 25)

  locally:
    import auto.semigroup.given
    validate("auto.semigroup")

  locally:
    import semiInstances.given
    validate("semiauto.semigroup")

  locally:
    import strictInstances.given
    validate("strict.semiauto.semigroup")
    testNoInstance("strict.semiauto.semigroup", "Top")

  locally:
    import derivedInstances.*
    val instances = "derived.semigroup"
    checkAll(s"$instances[Foo]", tests[Foo].semigroup)
    checkAll(s"$instances[Interleaved[Int]]", tests[Interleaved[Int]].semigroup)
    checkAll(s"$instances[BoxMul]", tests[BoxMul].semigroup)
    checkAll(s"$instances is Serializable", SerializableTests.serializable(Semigroup[Foo]))

end SemigroupSuite

object SemigroupSuite:
  import ADTs.*

  object semiInstances:
    given Semigroup[Foo] = semiauto.semigroup
    given Semigroup[Recursive] = semiauto.semigroup
    given Semigroup[Interleaved[Int]] = semiauto.semigroup
    given Semigroup[Box[Mul]] = semiauto.semigroup

  object strictInstances:
    given Semigroup[Foo] = strict.semiauto.semigroup
    given Semigroup[Recursive] = strict.semiauto.semigroup
    given Semigroup[Interleaved[Int]] = strict.semiauto.semigroup
    given Semigroup[Box[Mul]] = strict.semiauto.semigroup

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives Semigroup
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Semigroup
    case class BoxMul(x: Box[Mul]) derives Semigroup

  final case class Mul(value: Int)
  object Mul:
    given Semigroup[Mul] = (x, y) => Mul(x.value * y.value)

end SemigroupSuite
