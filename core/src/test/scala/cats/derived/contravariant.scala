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

import cats.instances.all._
import cats.laws.discipline.{ContravariantTests, SerializableTests}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

class ContravariantSuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtF[A] = GenericAdt[A => Boolean]
  type ListFToInt[A] = List[A => Int]
  type InterleavedF[A] = Interleaved[A => Int]
  type AndCharF[A] = (A => Int, Char)
  type TreeF[A] = Tree[A => Int]

  case class Pred[A](run: A => Boolean)
  case class TwiceNest[A](run: Pred[Pred[A]])
  case class ThriceNest[A](run: Pred[TwiceNest[A]])

  implicit def eqIntPred[A: Numeric: Choose, B: Eq]: Eq[A => B] = new Eq[A => B] {
    def diagonal[A](a: A): (A, A) = (a, a)
    override def eqv(x: A => B, y: A => B): Boolean =
      Gen.posNum[A]
        .sample
        .exists(diagonal[A] _ andThen (x *** y) andThen (Eq[B].eqv _).tupled)
  }

  def testContravariant(context: String)(
    implicit
    optList: Contravariant[OptPred],
    tree: Contravariant[TreeF],
    listPred: Contravariant[ListPred],
    genadt: Contravariant[GenericAdtF],
    ListFToInt: Contravariant[ListFToInt],
    interleaved: Contravariant[InterleavedF],
    andCharF: Contravariant[AndCharF]
  ): Unit = {
    checkAll(s"$context.Contravariant[OptPred]", ContravariantTests[OptPred].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[TreeF]", ContravariantTests[TreeF].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[ListPred]", ContravariantTests[ListPred].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[GenAdtF]", ContravariantTests[GenericAdtF].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[InterleavedF]", ContravariantTests[InterleavedF].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[AndCharF]", ContravariantTests[AndCharF].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant is Serializable", SerializableTests.serializable(Contravariant[TreeF]))

    test(s"$context.Contravariant.contramap is stack safe") {
      val n = 10000
      val largeBoxed = (1 until n).toList.map((j: Int) => (i: Int) => i + j)
      val actualBoxed = ListFToInt.contramap[Int, Int](largeBoxed)((j: Int) => j + 1)
      val expected = (3 until n + 2).toList
      assert(actualBoxed.map(_.apply(1)) == expected)
    }
  }

  {
    import auto.contravariant._
    testContravariant("auto")
  }

  {
    import cached.contravariant._
    testContravariant("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val optPred: Contravariant[OptPred] = semi.contravariant[OptPred]
    implicit val listPred: Contravariant[ListPred] = semi.contravariant[ListPred]
    implicit val thriceNest: Contravariant[GenericAdtF] = semi.contravariant[GenericAdtF]
    implicit val listSnocendo: Contravariant[ListFToInt] = semi.contravariant[ListFToInt]
    implicit val interleaveF: Contravariant[InterleavedF] = semi.contravariant[InterleavedF]
    implicit val andCharF: Contravariant[AndCharF] = semi.contravariant[AndCharF]
    implicit val treeF: Contravariant[TreeF] = semi.contravariant[TreeF]
    implicit val pred: Contravariant[Pred] = semi.contravariant[Pred]


    def run(): Unit = testContravariant("semi")
  }
}

