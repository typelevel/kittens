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

import cats.Monoid
import cats.kernel.laws.discipline.{MonoidTests, SerializableTests}
import scala.compiletime.*

class MonoidSuite extends KittensSuite:
  import MonoidSuite.*
  import ADTs.*

  inline def tests[A]: MonoidTests[A] =
    MonoidTests[A](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Foo]", tests[Foo].monoid)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].monoid)
    checkAll(s"$instance[Box[Mul]]", tests[Box[Mul]].monoid)
    checkAll(s"$instance[Recursive]", tests[Recursive].monoid)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Monoid[Foo]]))
    test(s"$instance respects existing instances") {
      val box = summonInline[Monoid[Box[Mul]]]
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
    }

  locally {
    import auto.monoid.given
    validate("auto.monoid")
  }

  locally {
    import semiInstances.given
    validate("semiauto.monoid")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.monoid"
    checkAll(s"$instance[Foo]", tests[Foo].monoid)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].monoid)
    checkAll(s"$instance[BoxMul]", tests[BoxMul].monoid)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Monoid[Foo]))
  }

end MonoidSuite

object MonoidSuite:
  import ADTs.*

  object semiInstances:
    given Monoid[Foo] = semiauto.monoid
    given Monoid[Recursive] = semiauto.monoid
    given Monoid[Interleaved[Int]] = semiauto.monoid
    given Monoid[Box[Mul]] = semiauto.monoid

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives Monoid
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Monoid
    case class BoxMul(x: Box[Mul]) derives Monoid

  final case class Mul(value: Int)
  object Mul:
    given Monoid[Mul] with
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)

end MonoidSuite
