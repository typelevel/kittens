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
import scala.compiletime.*

class ContravariantSuite extends KittensSuite:
  import ContravariantSuite.*
  import TestDefns.*

  inline def contravariantTests[F[_]]: ContravariantTests[F] = ContravariantTests[F](summonInline)

  inline def testContravariant(context: String): Unit =
    checkAll(s"$context.Contravariant[OptPred]", contravariantTests[OptPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[TreePred]", contravariantTests[TreePred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$context.Contravariant[ListPred]", contravariantTests[ListPred].contravariant[MiniInt, String, Boolean])
    checkAll(
      s"$context.Contravariant[GenericAdtPred]",
      contravariantTests[GenericAdtPred].contravariant[MiniInt, String, Boolean]
    )
    // TODO https://github.com/typelevel/kittens/issues/473
    // checkAll(
    //   s"$context.Contravariant[InterleavedPred]",
    //   ContravariantTests[InterleavedPred].contravariant[MiniInt, String, Boolean]
    // )
    checkAll(
      s"$context.Contravariant[AndCharPred]",
      contravariantTests[AndCharPred].contravariant[MiniInt, String, Boolean]
    )
    checkAll(
      s"$context.Contravariant[ListSnocF]",
      contravariantTests[ListSnocF].contravariant[MiniInt, String, Boolean]
    )
    checkAll(
      s"$context.Contravariant is Serializable",
      SerializableTests.serializable(summonInline[Contravariant[TreePred]])
    )

    // TODO https://github.com/typelevel/kittens/issues/476
    // test(s"$context.Contravariant.contramap is stack safe") {
    //   val C = summonInline[Contravariant[ListSnocF]]
    //   val n = 10000
    //   val largeBoxed = Snoc.fromSeq((1 until n).map((j: Int) => (i: Int) => i + j)) :: Nil
    //   val actualBoxed = C.contramap[Int, Int](largeBoxed)((j: Int) => j + 1).flatMap(Snoc.toList)
    //   val expected = (3 until n + 2).toList
    //   assert(actualBoxed.map(_.apply(1)) == expected)
    // }

  locally {
    import auto.contravariant.given
    testContravariant("auto")
  }

  locally {
    import semiInstances.given
    testContravariant("semiauto")
  }

object ContravariantSuite:
  import TestDefns._

  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtPred[A] = GenericAdt[A => Boolean]
  type ListSnocF[A] = List[Snoc[A => Int]]
  type InterleavedPred[A] = Interleaved[A => Boolean]
  type AndCharPred[A] = (A => Boolean, Char)
  type TreePred[A] = Tree[A => Boolean]

  object semiInstances:
    implicit val optPred: Contravariant[OptPred] = semiauto.contravariant
    implicit val treePred: Contravariant[TreePred] = semiauto.contravariant
    implicit val listPred: Contravariant[ListPred] = semiauto.contravariant
    implicit val genericAdtPred: Contravariant[GenericAdtPred] = semiauto.contravariant
    // implicit val interleavePred: Contravariant[InterleavedPred] = semiauto.contravariant
    implicit val andCharPred: Contravariant[AndCharPred] = semiauto.contravariant
    implicit val listSnocF: Contravariant[ListSnocF] = semiauto.contravariant

  case class Single[A](value: A) derives Functor

  enum Many[+A] derives Functor:
    case Naught
    case More(value: A, rest: Many[A])

  enum AtMostOne[+A] derives Functor:
    case Naught
    case Single(value: A)

  enum AtLeastOne[+A] derives Functor:
    case Single(value: A)
    case More(value: A, rest: Option[AtLeastOne[A]])
