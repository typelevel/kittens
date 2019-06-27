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

import cats.kernel.laws.discipline.{EqTests, SerializableTests}
import cats.instances.all._

class EqSuite extends KittensSuite {
  import TestDefns._

  def testEq(context: String)(
    implicit foo: Eq[Foo],
    iList: Eq[IList[Int]],
    inner: Eq[Inner],
    outer: Eq[Outer],
    interleaved: Eq[Interleaved[Int]],
    tree: Eq[Tree[Int]],
    recursive: Eq[Recursive]
  ): Unit = {
    checkAll(s"$context.Eq[Foo]]", EqTests[Foo].eqv)
    checkAll(s"$context.Eq[IList[Int]]", EqTests[IList[Int]].eqv)
    checkAll(s"$context.Eq[Inner]", EqTests[Inner].eqv)
    checkAll(s"$context.Eq[Outer]", EqTests[Outer].eqv)
    checkAll(s"$context.Eq[Interleaved[Int]]", EqTests[Interleaved[Int]].eqv)
    checkAll(s"$context.Eq[Tree[Int]]", EqTests[Tree[Int]].eqv)
    checkAll(s"$context.Eq[Recursive]", EqTests[Recursive].eqv)
    checkAll(s"$context.Eq is Serializable", SerializableTests.serializable(Eq[Tree[Int]]))
  }

  {
    import auto.eq._
    testEq("auto")
  }

  {
    import cached.eq._
    testEq("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val foo: Eq[Foo] = semi.eq
    implicit val iList: Eq[IList[Int]] = semi.eq
    implicit val inner: Eq[Inner] = semi.eq
    implicit val outer: Eq[Outer] = semi.eq
    implicit val interleaved: Eq[Interleaved[Int]] = semi.eq
    implicit val tree: Eq[Tree[Int]] = semi.eq
    implicit val recursive: Eq[Recursive] = semi.eq
    def run(): Unit = testEq("semi")
  }
}
