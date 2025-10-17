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
import cats.kernel.laws.discipline.*

import scala.annotation.unused
import scala.compiletime.*

class EqSuite extends KittensSuite.WithoutEq:
  import EqSuite.*
  import ADTs.*

  inline def tests[A]: EqTests[A] =
    EqTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Foo]]", tests[Foo].eqv)
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].eqv)
    checkAll(s"$instance[Inner]", tests[Inner].eqv)
    checkAll(s"$instance[Outer]", tests[Outer].eqv)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].eqv)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].eqv)
    checkAll(s"$instance[Recursive]", tests[Recursive].eqv)
    checkAll(s"$instance[Singletons[Int]]", tests[Singletons[Int]].eqv)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Eq[Foo]]))

  locally:
    import auto.eq.given
    validate("auto.eq")

  locally:
    import semiInstances.given
    validate("semiauto.eq")

  locally:
    import strictInstances.given
    validate("strict.semiauto.eq")
    testNoInstance("strict.semiauto.eq", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.eq"
    checkAll(s"$instance[Foo]]", tests[Foo].eqv)
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].eqv)
    checkAll(s"$instance[Inner]", tests[Inner].eqv)
    checkAll(s"$instance[Outer]", tests[Outer].eqv)
    checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].eqv)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].eqv)
    checkAll(s"$instance[Recursive]", tests[Recursive].eqv)
    checkAll(s"$instance[Singletons[Int]]", tests[Singletons[Int]].eqv)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Eq[Foo]))

end EqSuite

object EqSuite:
  import ADTs.*

  object semiInstances:
    given Eq[Foo] = semiauto.eq
    given Eq[IList[Int]] = semiauto.eq
    given Eq[Inner] = semiauto.eq
    given Eq[Outer] = semiauto.eq
    given Eq[Interleaved[Int]] = semiauto.eq
    given Eq[Tree[Int]] = semiauto.eq
    given Eq[Recursive] = semiauto.eq
    given Eq[Singletons[Int]] = semiauto.eq

  object strictInstances:
    @unused given [A <: Singleton: ValueOf]: Eq[A] = Eq.allEqual
    given Eq[Foo] = strict.semiauto.eq
    given Eq[IList[Int]] = strict.semiauto.eq
    given Eq[Inner] = strict.semiauto.eq
    given Eq[Outer] = strict.semiauto.eq
    given Eq[Interleaved[Int]] = strict.semiauto.eq
    given Eq[Tree[Int]] = strict.semiauto.eq
    given Eq[Recursive] = strict.semiauto.eq
    given Eq[Singletons[Int]] = strict.semiauto.eq

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives Eq
    case class IList[A](x: ADTs.IList[A]) derives Eq
    case class Inner(x: ADTs.Inner) derives Eq
    case class Outer(x: ADTs.Outer) derives Eq
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Eq
    case class Tree[A](x: ADTs.Tree[A]) derives Eq
    case class Recursive(x: ADTs.Recursive) derives Eq
    case class Singletons[A](x: ADTs.Singletons[A]) derives Eq

end EqSuite
