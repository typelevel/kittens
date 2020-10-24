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

import cats.NonEmptyTraverse
import cats.data.{NonEmptyList, NonEmptyVector, OneAnd}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{NonEmptyTraverseTests, SerializableTests}

class NonEmptyTraverseSuite extends KittensSuite {
  import NonEmptyTraverseSuite._
  import TestDefns._
  import TestEqInstances._

  def testReducible(context: String)(implicit
      iCons: NonEmptyTraverse[ICons],
      tree: NonEmptyTraverse[Tree],
      nelSCons: NonEmptyTraverse[NelSCons],
      nelAndOne: NonEmptyTraverse[NelAndOne],
      listAndNel: NonEmptyTraverse[ListAndNel],
      interleaved: NonEmptyTraverse[Interleaved]
  ): Unit = {

    checkAll(
      s"$context.NonEmptyTraverse[ICons]",
      NonEmptyTraverseTests[ICons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse[Tree]",
      NonEmptyTraverseTests[Tree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse[NelSCons]",
      NonEmptyTraverseTests[NelSCons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse[NelAndOne]",
      NonEmptyTraverseTests[NelAndOne].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse[ListAndNel]",
      NonEmptyTraverseTests[ListAndNel].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse[Interleaved]",
      NonEmptyTraverseTests[Interleaved].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )

    checkAll(
      s"$context.NonEmptyTraverse is Serializable",
      SerializableTests.serializable(NonEmptyTraverse[Tree])
    )

    val n = 10000
    val largeIList = ICons(0, IList.fromSeq(1 until n))
    val largeSnoc: NelSCons[Int] = NonEmptyList.one(SCons(Snoc.fromSeq(1 until n), 0))

    test(s"$context.Traverse.nonEmptyTraverse is stack safe") {
      val actualIList = largeIList.nonEmptyTraverse(i => Option(i + 1)).map(IList.toList)
      val actualSnoc = largeSnoc.nonEmptyTraverse(i => Option(i + 1)).map(_.toList.flatMap(Snoc.toList))
      assert(actualIList.isDefined)
      assert(actualSnoc.isDefined)
    }
  }

  {
    import auto.nonEmptyTraverse._
    testReducible("auto")
  }

  {
    import cached.nonEmptyTraverse._
    testReducible("cached")
  }

  {
    import semiInstances._
    testReducible("semiauto")
  }
}

object NonEmptyTraverseSuite {
  import TestDefns._

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[List, A]]
  type ListAndNel[A] = (List[A], NonEmptyList[A])

  object semiInstances {
    implicit val iCons: NonEmptyTraverse[ICons] = semiauto.nonEmptyTraverse
    implicit val tree: NonEmptyTraverse[Tree] = semiauto.nonEmptyTraverse
    implicit val nelSCons: NonEmptyTraverse[NelSCons] = semiauto.nonEmptyTraverse
    implicit val nelAndOne: NonEmptyTraverse[NelAndOne] = semiauto.nonEmptyTraverse
    implicit val listAndNel: NonEmptyTraverse[ListAndNel] = semiauto.nonEmptyTraverse
    implicit val interleaved: NonEmptyTraverse[Interleaved] = semiauto.nonEmptyTraverse
  }
}
