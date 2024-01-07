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

import cats.Invariant
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import shapeless3.deriving.Const

import scala.compiletime.*

class InvariantSuite extends KittensSuite:
  import ADTs.*
  import InvariantSuite.*

  inline def tests[F[_]]: InvariantTests[F] =
    InvariantTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[TreeF]", tests[TreeF].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[GenericAdtF]", tests[GenericAdtF].invariant[MiniInt, String, Boolean])
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
    checkAll(s"$instance[Singletons]", tests[Singletons].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Search]", tests[Search].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Invariant[TreeF]]))

  locally:
    import auto.invariant.given
    validate("auto.invariant")

  locally:
    import semiInstances.given
    validate("semiauto.invariant")

  locally:
    import strictInstances.given
    validate("strict.semiauto.invariant")
    testNoInstance("strict.semiauto.invariant", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.invariant"
    checkAll(s"$instance[IList]", tests[IList].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Bivariant]", tests[Bivariant].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[EnumK1Inv]", tests[EnumK1Inv].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Many]", tests[Many].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance[Singletons]", tests[Singletons].invariant[MiniInt, String, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Invariant[AtMostOne]))

end InvariantSuite

object InvariantSuite:
  import ADTs.*

  type ListSnoc[A] = List[Snoc[A]]
  type GenericAdtF[A] = GenericAdt[A => Boolean]
  type ListFToInt[A] = List[Snoc[A => Int]]
  type InterleavedF[A] = Interleaved[A => Boolean]
  type AndCharF[A] = (A => Boolean, Char)
  type TreeF[A] = Tree[A => Boolean]

  object semiInstances:
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
    given Invariant[Singletons] = semiauto.invariant
    given Invariant[Search] = semiauto.invariant

  object strictInstances:
    given [T]: Invariant[Const[T]] = strict.semiauto.invariant
    given [F[_]: Invariant, G[_]: Invariant]: Invariant[[x] =>> F[G[x]]] = Invariant[F].compose[G]
    given [F[_]: Invariant, R]: Invariant[[x] =>> F[x => R]] = Invariant[F].compose[[x] =>> x => R]
    given Invariant[Snoc] = strict.semiauto.invariant
    given Invariant[GenericAdtF] = strict.semiauto.invariant
    given Invariant[ListFToInt] = strict.semiauto.invariant
    // TODO: https://github.com/typelevel/kittens/issues/473
    // given Invariant[InterleavedF] = strict.semiauto.invariant
    given Invariant[AndCharF] = strict.semiauto.invariant
    given Invariant[TreeF] = strict.semiauto.invariant
    given Invariant[Pred] = strict.semiauto.invariant
    given Invariant[ListSnoc] = strict.semiauto.invariant
    given Invariant[Bivariant] = strict.semiauto.invariant
    given Invariant[IList] = strict.semiauto.invariant
    given Invariant[EnumK1Inv] = strict.semiauto.invariant
    given Invariant[Many] = strict.semiauto.invariant
    given Invariant[AtMostOne] = strict.semiauto.invariant
    given Invariant[AtLeastOne] = strict.semiauto.invariant
    given Invariant[Singletons] = strict.semiauto.invariant
    given Invariant[Search] = strict.semiauto.invariant

  object derivedInstances:
    case class Bivariant[A](x: ADTs.Bivariant[A]) derives Invariant
    case class IList[A](x: ADTs.IList[A]) derives Invariant
    case class EnumK1Inv[A](x: ADTs.EnumK1Inv[A]) derives Invariant
    case class Many[A](x: ADTs.Many[A]) derives Invariant
    case class AtMostOne[A](x: ADTs.AtMostOne[A]) derives Invariant
    case class AtLeastOne[A](x: ADTs.AtLeastOne[A]) derives Invariant
    case class Singletons[A](x: ADTs.Singletons[A]) derives Invariant

end InvariantSuite
