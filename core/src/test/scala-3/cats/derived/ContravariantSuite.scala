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
import scala.compiletime.*

class ContravariantSuite extends KittensSuite:
  import ContravariantSuite.*
  import TestDefns.*

  inline def tests[F[_]]: ContravariantTests[F] =
    ContravariantTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[OptPred]", tests[OptPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[TreePred]", tests[TreePred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[ListPred]", tests[ListPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[GenericAdtPred]", tests[GenericAdtPred].contravariant[MiniInt, String, Boolean])
    // TODO https://github.com/typelevel/kittens/issues/473
    // checkAll(s"instance[InterleavedPred]", tests[InterleavedPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AndCharPred]", tests[AndCharPred].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[ListSnocF]", tests[ListSnocF].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance[EnumK1Contra]", tests[EnumK1Contra].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Contravariant[TreePred]]))
    // TODO https://github.com/typelevel/kittens/issues/476
    // test(s"instance.contramap is stack safe") {
    //   val C = summonInline[Contravariant[ListSnocF]]
    //   val n = 10000
    //   val largeBoxed = Snoc.fromSeq((1 until n).map((j: Int) => (i: Int) => i + j)) :: Nil
    //   val actualBoxed = C.contramap[Int, Int](largeBoxed)((j: Int) => j + 1).flatMap(Snoc.toList)
    //   val expected = (3 until n + 2).toList
    //   assert(actualBoxed.map(_.apply(1)) == expected)
    // }

  locally {
    import auto.contravariant.given
    validate("auto.contravariant")
  }

  locally {
    import semiContravariant.given
    validate("semiauto.contravariant")
  }

  locally {
    import derivedContravariant.*
    val instance = "derived.contravariant"
    checkAll(s"$instance[EnumK1Contra]", tests[EnumK1Contra].contravariant[MiniInt, String, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Contravariant[EnumK1Contra]]))
  }

end ContravariantSuite

object ContravariantSuite:
  import TestDefns.*

  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtPred[A] = GenericAdt[A => Boolean]
  type ListSnocF[A] = List[Snoc[A => Int]]
  type InterleavedPred[A] = Interleaved[A => Boolean]
  type AndCharPred[A] = (A => Boolean, Char)
  type TreePred[A] = Tree[A => Boolean]

  object semiContravariant:
    given Contravariant[OptPred] = semiauto.contravariant
    given Contravariant[TreePred] = semiauto.contravariant
    given Contravariant[ListPred] = semiauto.contravariant
    given Contravariant[GenericAdtPred] = semiauto.contravariant
    // given Contravariant[InterleavedPred] = semiauto.contravariant
    given Contravariant[AndCharPred] = semiauto.contravariant
    given Contravariant[ListSnocF] = semiauto.contravariant
    given Contravariant[EnumK1Contra] = semiauto.contravariant

  object derivedContravariant:
    case class EnumK1Contra[-A](x: TestDefns.EnumK1Contra[A]) derives Contravariant

    enum Many[-A] derives Contravariant:
      case Naught
      case More(value: A => Unit, rest: Many[A])

    enum AtMostOne[-A] derives Contravariant:
      case Naught
      case Single(value: A => Unit)

    enum AtLeastOne[-A] derives Contravariant:
      case Single(value: A => Unit)
      case More(value: A => Unit, rest: Option[AtLeastOne[A]])

end ContravariantSuite
