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
import cats.kernel.laws.discipline.{EqTests, SerializableTests}

import scala.compiletime.*

class EqSuite extends KittensSuite.WithoutEq:
  import EqSuite.*
  import TestDefns.*

  inline def eqTests[A]: EqTests[A] =
    EqTests[A](summonInline)

  inline def testEq(inline context: String): Unit =
    checkAll(s"$context.Eq[Foo]]", eqTests[Foo].eqv)
    checkAll(s"$context.Eq[IList[Int]]", eqTests[IList[Int]].eqv)
    checkAll(s"$context.Eq[Inner]", eqTests[Inner].eqv)
    checkAll(s"$context.Eq[Outer]", eqTests[Outer].eqv)
    checkAll(s"$context.Eq[Interleaved[Int]]", eqTests[Interleaved[Int]].eqv)
    checkAll(s"$context.Eq[Tree[Int]]", eqTests[Tree[Int]].eqv)
    checkAll(s"$context.Eq[Recursive]", eqTests[Recursive].eqv)
    checkAll(s"$context.Eq is Serializable", SerializableTests.serializable(summonInline[Eq[Foo]]))

  locally {
    import auto.eq.given
    testEq("auto")
  }

  locally {
    import semiInstances.given
    testEq("semiauto")
  }

end EqSuite

object EqSuite:
  import TestDefns.*

  object semiInstances:
    given Eq[Foo] = semiauto.eq
    given Eq[IList[Int]] = semiauto.eq
    given Eq[Inner] = semiauto.eq
    given Eq[Outer] = semiauto.eq
    given Eq[Interleaved[Int]] = semiauto.eq
    given Eq[Tree[Int]] = semiauto.eq
    given Eq[Recursive] = semiauto.eq

end EqSuite
