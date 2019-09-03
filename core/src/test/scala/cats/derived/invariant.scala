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

import cats.implicits._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline._

class InvariantSuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtF[A] = GenericAdt[A => Boolean]
  type ListFToInt[A] = List[Snoc[A => Int]]
  type InterleavedF[A] = Interleaved[A => Boolean]
  type AndCharF[A] = (A => Boolean, Char)
  type TreeF[A] = Tree[A => Boolean]

  case class Pred[A](run: A => Boolean)

  def testInvariant(context: String)(
    implicit
    optList: Invariant[OptPred],
    tree: Invariant[TreeF],
    listPred: Invariant[ListPred],
    genadt: Invariant[GenericAdtF],
    ListFToInt: Invariant[ListFToInt],
    interleaved: Invariant[InterleavedF],
    andCharF: Invariant[AndCharF]
  ): Unit = {
    checkAll(s"$context.Invariant[OptPred]", InvariantTests[OptPred].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[TreeF]", InvariantTests[TreeF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[ListPred]", InvariantTests[ListPred].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[GenAdtF]", InvariantTests[GenericAdtF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[InterleavedF]", InvariantTests[InterleavedF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[AndCharF]", InvariantTests[AndCharF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant is Serializable", SerializableTests.serializable(Invariant[TreeF]))
  }

  {
    import auto.invariant._
    testInvariant("auto")
  }

  {
    import cached.invariant._
    testInvariant("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val optPred: Invariant[OptPred] = semi.invariant[OptPred]
    implicit val listPred: Invariant[ListPred] = semi.invariant[ListPred]
    implicit val gadt: Invariant[GenericAdtF] = semi.invariant[GenericAdtF]
    implicit val listSnocendo: Invariant[ListFToInt] = semi.invariant[ListFToInt]
    implicit val interleaveF: Invariant[InterleavedF] = semi.invariant[InterleavedF]
    implicit val andCharF: Invariant[AndCharF] = semi.invariant[AndCharF]
    implicit val treeF: Invariant[TreeF] = semi.invariant[TreeF]
    implicit val pred: Invariant[Pred] = semi.invariant[Pred]

    def run(): Unit = testInvariant("semi")
  }
}

