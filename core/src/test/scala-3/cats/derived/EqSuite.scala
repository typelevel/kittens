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

  inline def tests[A]: EqTests[A] =
    EqTests[A](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Foo]]", tests[Foo].eqv)
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].eqv)
    checkAll(s"$instance[Inner]", tests[Inner].eqv)
    checkAll(s"$instance[Outer]", tests[Outer].eqv)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].eqv)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].eqv)
    checkAll(s"$instance[Recursive]", tests[Recursive].eqv)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Eq[Foo]]))

  locally {
    import auto.eq.given
    validate("auto.eq")
  }

  locally {
    import semiEq.given
    validate("semiauto.eq")
  }

  locally {
    import derivedEq.*
    val instance = "derived.eq"
    checkAll(s"$instance[Foo]]", tests[Foo].eqv)
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].eqv)
    checkAll(s"$instance[Inner]", tests[Inner].eqv)
    checkAll(s"$instance[Outer]", tests[Outer].eqv)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].eqv)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].eqv)
    checkAll(s"$instance[Recursive]", tests[Recursive].eqv)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Eq[Foo]]))
  }

end EqSuite

object EqSuite:
  import TestDefns.*

  object semiEq:
    given Eq[Foo] = semiauto.eq
    given Eq[IList[Int]] = semiauto.eq
    given Eq[Inner] = semiauto.eq
    given Eq[Outer] = semiauto.eq
    given Eq[Interleaved[Int]] = semiauto.eq
    given Eq[Tree[Int]] = semiauto.eq
    given Eq[Recursive] = semiauto.eq

  object derivedEq:
    case class Foo(x: TestDefns.Foo) derives Eq
    case class IList[A](x: TestDefns.IList[A]) derives Eq
    case class Inner(x: TestDefns.Inner) derives Eq
    case class Outer(x: TestDefns.Outer) derives Eq
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Eq
    case class Tree[A](x: TestDefns.Tree[A]) derives Eq
    case class Recursive(x: TestDefns.Recursive) derives Eq

end EqSuite
