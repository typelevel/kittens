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

import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.*

import scala.compiletime.*

class InvariantSuite extends KittensSuite:
  import InvariantSuite.*
  import TestDefns.*

  inline def invariantTests[F[_]]: InvariantTests[F] =
    InvariantTests[F](summonInline)

  inline def testInvariant(context: String): Unit = {
    checkAll(s"$context.Invariant[TreeF]", invariantTests[TreeF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[GenAdtF]", invariantTests[GenericAdtF].invariant[MiniInt, String, Boolean])
    // TODO https://github.com/typelevel/kittens/issues/473
    // checkAll(s"$context.Invariant[InterleavedF]", invariantTests[InterleavedF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[AndCharF]", invariantTests[AndCharF].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[ListSnoc", invariantTests[ListSnoc].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[Bivariant]", invariantTests[Bivariant].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant[EnumK1Inv]", invariantTests[EnumK1Inv].invariant[MiniInt, String, Boolean])
    checkAll(s"$context.Invariant is Serializable", SerializableTests.serializable(summonInline[Invariant[TreeF]]))

    // TODO https://github.com/typelevel/kittens/issues/476
    // test(s"$context.Invariant.imap is stack safe") {
    //   val I = summonInline[Invariant[ListSnoc]]
    //   val J = summonInline[Invariant[IList]]
    //   val n = 10000
    //   val largeIList = IList.fromSeq(1 until n)
    //   val largeSnoc = Snoc.fromSeq(1 until n) :: Nil
    //   val actualIList = IList.toList(J.imap(largeIList)(_ + 1)(_ - 1))
    //   val actualSnoc = I.imap(largeSnoc)(_ + 1)(_ - 1).flatMap(Snoc.toList)
    //   val expected = (2 until n + 1).toList
    //   assert(actualIList == expected)
    //   assert(actualSnoc == expected)
    // }
  }

  locally {
    import auto.invariant.given
    testInvariant("auto")
  }

  locally {
    import semiInstances.given
    testInvariant("semiauto")
  }

end InvariantSuite

object InvariantSuite:
  import TestDefns.*

  type ListSnoc[A] = List[Snoc[A]]
  type GenericAdtF[A] = GenericAdt[A => Boolean]
  type ListFToInt[A] = List[Snoc[A => Int]]
  type InterleavedF[A] = Interleaved[A => Boolean]
  type AndCharF[A] = (A => Boolean, Char)
  type TreeF[A] = Tree[A => Boolean]

  object semiInstances:
    given Invariant[GenericAdtF] = semiauto.invariant
    given Invariant[ListFToInt] = semiauto.invariant
    // given Invariant[InterleavedF] = semiauto.invariant
    given Invariant[AndCharF] = semiauto.invariant
    given Invariant[TreeF] = semiauto.invariant
    given Invariant[Pred] = semiauto.invariant
    given Invariant[ListSnoc] = semiauto.invariant
    given Invariant[Bivariant] = semiauto.invariant
    given Invariant[IList] = semiauto.invariant
    given Invariant[EnumK1Inv] = semiauto.invariant

  case class Single[A](value: A) derives Invariant

  enum Many[A] derives Invariant:
    case Naught()
    case More(value: A, rest: Many[A])

  enum AtMostOne[A] derives Invariant:
    case Naught()
    case Single(value: A)

  enum AtLeastOne[A] derives Invariant:
    case Single(value: A)
    case More(value: A, rest: Option[AtLeastOne[A]])

end InvariantSuite
