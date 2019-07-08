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

class ContravariantSuite extends KittensSuite {
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

  def testContravariant(context: String)(
    implicit
    optList: Contravariant[OptPred],
    tree: Contravariant[TreeF],
    listPred: Contravariant[ListPred],
    genadt: Contravariant[GenericAdtF],
    ListFToInt: Contravariant[ListFToInt],
    interleaved: Contravariant[InterleavedF],
    andCharF: Contravariant[AndCharF],
    thriceNest: Contravariant[Lambda[A => Pred[Pred[Pred[A]]]]]
  ): Unit = {
    checkAll(s"$context.Contravariant[OptPred]", ContravariantTests[OptPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[TreeF]", ContravariantTests[TreeF].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[ListPred]", ContravariantTests[ListPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[GenAdtF]", ContravariantTests[GenericAdtF].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[InterleavedF]", ContravariantTests[InterleavedF].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[AndCharF]", ContravariantTests[AndCharF].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant is Serializable", SerializableTests.serializable(Contravariant[TreeF]))

    test(s"$context.Contravariant.contramap is stack safe") {
      val n = 10000
      val largeBoxed = Snoc.fromSeq((1 until n).map((j: Int) => (i: Int) => i + j)) :: Nil
      val actualBoxed = ListFToInt.contramap[Int, Int](largeBoxed)((j: Int) => j + 1).flatMap(Snoc.toList)
      val expected = (3 until n + 2).toList
      assert(actualBoxed.map(_.apply(1)) == expected)
    }
  }

  {
    import auto.functor._
    import auto.contravariant._
    testContravariant("auto")
  }

  {
    import cached.functor._
    import cached.contravariant._
    testContravariant("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val optPred: Contravariant[OptPred] = semi.contravariant[OptPred]
    implicit val listPred: Contravariant[ListPred] = semi.contravariant[ListPred]
    implicit val gadt: Contravariant[GenericAdtF] = semi.contravariant[GenericAdtF]
    implicit val listSnocendo: Contravariant[ListFToInt] = semi.contravariant[ListFToInt]
    implicit val interleaveF: Contravariant[InterleavedF] = semi.contravariant[InterleavedF]
    implicit val andCharF: Contravariant[AndCharF] = semi.contravariant[AndCharF]
    implicit val treeF: Contravariant[TreeF] = semi.contravariant[TreeF]
    implicit val pred: Contravariant[Pred] = semi.contravariant[Pred]
    implicit val twiceNest: Functor[Lambda[A => Pred[Pred[A]]]] = semi.functor[Lambda[A => Pred[Pred[A]]]]
    implicit val thriceNest: Contravariant[Lambda[A => Pred[Pred[Pred[A]]]]] = semi.contravariant[Lambda[A => Pred[Pred[Pred[A]]]]]

    def run(): Unit = testContravariant("semi")
  }
}

