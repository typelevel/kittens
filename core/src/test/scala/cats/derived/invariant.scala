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
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline._

class InvariantSuite extends KittensSuite {
  import InvariantSuite._
  import TestDefns._
  import TestEqInstances._

  def testInvariant(context: String)(
    implicit
    tree: Invariant[TreeF],
    genadt: Invariant[GenericAdtF],
    ListFToInt: Invariant[ListFToInt],
    listSnoc: Invariant[ListSnoc],
    interleaved: Invariant[InterleavedF],
    andCharF: Invariant[AndCharF],
    bivariant: Invariant[Bivariant],
    ilist: Invariant[IList]
  ): Unit = {
    checkAll(s"$context.Invariant[TreeF]", InvariantTests[TreeF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[GenAdtF]", InvariantTests[GenericAdtF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[InterleavedF]", InvariantTests[InterleavedF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[AndCharF]", InvariantTests[AndCharF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[ListSnoc", InvariantTests[ListSnoc].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[Bivariant]", InvariantTests[Bivariant].invariant[MiniInt, String, Boolean])

    checkAll(s"$context.Invariant is Serializable", SerializableTests.serializable(Invariant[TreeF]))

    test(s"$context.Invariant.imap is stack safe") {
      val n = 10000
      val largeIList = IList.fromSeq(1 until n)
      val largeSnoc = Snoc.fromSeq(1 until n) :: Nil
      val actualIList = IList.toList(largeIList.imap(_ + 1)(_ - 1))
      val actualSnoc = listSnoc.imap(largeSnoc)(_ + 1)(_ - 1).flatMap(Snoc.toList)
      val expected = (2 until n + 1).toList
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }
  }

  {
    import auto.invariant._
    testInvariant("auto")
  }

  {
    import cached.invariant._
    testInvariant("cached")
  }

  {
    import semiInstances._
    testInvariant("semiauto")
  }
}

object InvariantSuite {
  import TestDefns._

  type ListSnoc[A] = List[Snoc[A]]
  type GenericAdtF[A] = GenericAdt[A => Boolean]
  type ListFToInt[A] = List[Snoc[A => Int]]
  type InterleavedF[A] = Interleaved[A => Boolean]
  type AndCharF[A] = (A => Boolean, Char)
  type TreeF[A] = Tree[A => Boolean]

  object semiInstances {
    implicit val gadt: Invariant[GenericAdtF] = semiauto.invariant[GenericAdtF]
    implicit val listSnocendo: Invariant[ListFToInt] = semiauto.invariant[ListFToInt]
    implicit val interleaveF: Invariant[InterleavedF] = semiauto.invariant[InterleavedF]
    implicit val andCharF: Invariant[AndCharF] = semiauto.invariant[AndCharF]
    implicit val treeF: Invariant[TreeF] = semiauto.invariant[TreeF]
    implicit val pred: Invariant[Pred] = semiauto.invariant[Pred]
    implicit val snoc: Invariant[ListSnoc] = semiauto.invariant[ListSnoc]
    implicit val bivariant: Invariant[Bivariant] = semiauto.invariant[Bivariant]
    implicit val ilist: Invariant[IList] = semiauto.invariant[IList]
  }
}
