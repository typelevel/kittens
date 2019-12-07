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

class ContravariantSuite extends KittensSuite {
  import ContravariantSuite._
  import TestDefns._
  import TestEqInstances._

  def testContravariant(context: String)(
    implicit
    optPred: Contravariant[OptPred],
    treePred: Contravariant[TreePred],
    listPred: Contravariant[ListPred],
    genericAdtPred: Contravariant[GenericAdtPred],
    liftSnocF: Contravariant[ListSnocF],
    interleavedPred: Contravariant[InterleavedPred],
    andCharPred: Contravariant[AndCharPred]
  ): Unit = {
    checkAll(s"$context.Contravariant[OptPred]", ContravariantTests[OptPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[TreePred]", ContravariantTests[TreePred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[ListPred]", ContravariantTests[ListPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[GenericAdtPred]", ContravariantTests[GenericAdtPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[InterleavedPred]", ContravariantTests[InterleavedPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[AndCharPred]", ContravariantTests[AndCharPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant is Serializable", SerializableTests.serializable(Contravariant[TreePred]))

    test(s"$context.Contravariant.contramap is stack safe") {
      val n = 10000
      val largeBoxed = Snoc.fromSeq((1 until n).map((j: Int) => (i: Int) => i + j)) :: Nil
      val actualBoxed = liftSnocF.contramap[Int, Int](largeBoxed)((j: Int) => j + 1).flatMap(Snoc.toList)
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

  {
    import semiInstances._
    testContravariant("semiauto")
  }
}

object ContravariantSuite {
  import TestDefns._

  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtPred[A] = GenericAdt[A => Boolean]
  type ListSnocF[A] = List[Snoc[A => Int]]
  type InterleavedPred[A] = Interleaved[A => Boolean]
  type AndCharPred[A] = (A => Boolean, Char)
  type TreePred[A] = Tree[A => Boolean]

  object semiInstances {
    implicit val optPred: Contravariant[OptPred] = semiauto.contravariant
    implicit val listPred: Contravariant[ListPred] = semiauto.contravariant
    implicit val genericAdtPred: Contravariant[GenericAdtPred] = semiauto.contravariant
    implicit val listSnocF: Contravariant[ListSnocF] = semiauto.contravariant
    implicit val interleavePred: Contravariant[InterleavedPred] = semiauto.contravariant
    implicit val andCharPred: Contravariant[AndCharPred] = semiauto.contravariant
    implicit val treePred: Contravariant[TreePred] = semiauto.contravariant
  }
}

