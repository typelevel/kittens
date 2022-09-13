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

  inline def tests[F[_]]: InvariantTests[F] =
    InvariantTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[TreeF]", tests[TreeF].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[GenAdtF]", tests[GenericAdtF].invariant[MiniInt, String, Boolean])
    // TODO: https://github.com/typelevel/kittens/issues/473
    // checkAll(s"$instance[InterleavedF]", tests[InterleavedF].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AndCharF]", tests[AndCharF].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[ListSnoc]", tests[ListSnoc].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[IList]", tests[IList].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Bivariant]", tests[Bivariant].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[EnumK1Inv]", tests[EnumK1Inv].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Many]", tests[Many].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Invariant[TreeF]]))

  locally {
    import auto.invariant.given
    validate("auto.invariant")
  }

  locally {
    import semiInvariant.given
    validate("semiauto.invariant")
  }

  locally {
    import derivedInvariant.*
    val instance = "derived.invariant"
    checkAll(s"$instance[IList]", tests[IList].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Bivariant]", tests[Bivariant].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[EnumK1Inv]", tests[EnumK1Inv].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Many]", tests[Many].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].invariant[MiniInt, String, Boolean])
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

  object semiInvariant:
    given Invariant[GenericAdtF] = semiauto.invariant
    given Invariant[ListFToInt] = semiauto.invariant
    // TODO: https://github.com/typelevel/kittens/issues/473
    // given Invariant[InterleavedF] = semiauto.invariant
    given Invariant[AndCharF] = semiauto.invariant
    given Invariant[TreeF] = semiauto.invariant
    given Invariant[Pred] = semiauto.invariant
    given Invariant[ListSnoc] = semiauto.invariant
    given Invariant[Bivariant] = semiauto.invariant
    given Invariant[IList] = semiauto.invariant
    given Invariant[EnumK1Inv] = semiauto.invariant
    given Invariant[Many] = semiauto.invariant
    given Invariant[AtMostOne] = semiauto.invariant
    given Invariant[AtLeastOne] = semiauto.invariant

  object derivedInvariant:
    case class Bivariant[A](x: TestDefns.Bivariant[A]) derives Invariant
    case class IList[A](x: TestDefns.IList[A]) derives Invariant
    case class EnumK1Inv[A](x: TestDefns.EnumK1Inv[A]) derives Invariant
    case class Many[A](x: TestDefns.Many[A]) derives Invariant
    case class AtMostOne[A](x: TestDefns.AtMostOne[A]) derives Invariant
    case class AtLeastOne[A](x: TestDefns.AtLeastOne[A]) derives Invariant

end InvariantSuite
