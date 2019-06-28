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
import cats.laws.discipline.ContravariantTests
import org.scalacheck.Gen
import org.scalacheck.Gen._

class ContravariantSuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  implicit val eqOptPredInt: Eq[Option[Int => Boolean]] = new Eq[OptPred[Int]] {
    override def eqv(x: OptPred[Int], y: OptPred[Int]): Boolean =
      Gen.chooseNum(0, 1000).sample.flatMap(
        sampled => Apply[Option].map2(x, y)({
          case (f, g) => f(sampled) == g(sampled)
        })
      ).exists(identity)
  }
  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type ThriceNest[A] = A => (A => (A => Boolean))

  def testContravariant(context: String)(
    implicit
    optList: Contravariant[OptPred],
    listSnoc: Contravariant[ListPred],
    thriceNest: Contravariant[ThriceNest]
  ): Unit = {
    checkAll(s"$context.Contravariant[OptList]", ContravariantTests[OptPred].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[ListSnoc]", ContravariantTests[ListPred].contravariant[Int, String, Long])
    checkAll(s"$context.Contravariant[AndChar]", ContravariantTests[ThriceNest].contravariant[Int, String, Long])

    test(s"$context.Contravariant.contramap is stack safe") {
//      val n = 10000
//      val largeIList = IList.fromSeq(1 until n)
//      val largeSnoc = Snoc.fromSeq(1 until n) :: Nil
//      val actualIList = IList.toList(largeIList.map(_ + 1))
//      val actualSnoc = listSnoc.map(largeSnoc)(_ + 1).flatMap(Snoc.toList)
//      val expected = (2 until n + 1).toList
//      assert(actualIList == expected)
//      assert(actualSnoc == expected)
    }
  }

//  {
//    import auto.contravariant._
//    testContravariant("auto")
//  }
//
//  {
//    import cached.contravariant._
//    testContravariant("cached")
//  }

  semiTests.run()

  object semiTests {
    implicit val optList: Contravariant[OptPred] = semi.contravariant
    implicit val listSnoc: Contravariant[ListPred] = semi.contravariant
    implicit val andChar: Contravariant[ThriceNest] = semi.contravariant
    def run(): Unit = testContravariant("semi")
  }
}

