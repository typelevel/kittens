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

import cats.{NonEmptyTraverse, Traverse}
import cats.data.{NonEmptyList, OneAnd}
import cats.derived.Derived.<<<
import cats.laws.discipline.*
import shapeless3.deriving.Const

import scala.compiletime.*

class NonEmptyTraverseSuite extends KittensSuite:
  import NonEmptyTraverseSuite.*
  import ADTs.*

  inline def tests[F[_]]: NonEmptyTraverseTests[F] =
    NonEmptyTraverseTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(
      s"$instance[ICons]",
      tests[ICons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Tree]",
      tests[Tree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[NelSCons]",
      tests[NelSCons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[NelAndOne]",
      tests[NelAndOne].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[VecAndNel]",
      tests[VecAndNel].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Interleaved]",
      tests[Interleaved].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[EnumK1]",
      tests[EnumK1].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[AtLeastOne]",
      tests[AtLeastOne].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Singletons]",
      tests[Singletons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Search]",
      tests[Search].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance is Serializable",
      SerializableTests.serializable(summonInline[NonEmptyTraverse[Tree]])
    )

  locally:
    import auto.nonEmptyTraverse.given
    validate("auto.nonEmptyTraverse")

  locally:
    import semiInstances.given
    validate("semiauto.nonEmptyTraverse")

  locally:
    import strictInstances.given
    validate("strict.semiauto.nonEmptyTraverse")
    testNoInstance("strict.semiauto.nonEmptyTraverse", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.nonEmptyTraverse"
    checkAll(
      s"$instance[ICons]",
      tests[ICons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Tree]",
      tests[Tree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Interleaved]",
      tests[Interleaved].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[EnumK1]",
      tests[EnumK1].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[AtLeastOne]",
      tests[AtLeastOne].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$instance[Singletons]",
      tests[Singletons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(s"$instance is Serializable", SerializableTests.serializable(NonEmptyTraverse[Tree]))

end NonEmptyTraverseSuite

object NonEmptyTraverseSuite:
  import ADTs.*

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[Vector, A]]
  type VecAndNel[A] = (Vector[A], NonEmptyList[A])

  object semiInstances:
    given NonEmptyTraverse[ICons] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Tree] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelSCons] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelAndOne] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[VecAndNel] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Interleaved] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[EnumK1] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[AtLeastOne] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Singletons] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Search] = semiauto.nonEmptyTraverse

  object strictInstances:
    given [T]: Traverse[Const[T]] = semiauto.traverse
    given [F[_]: Traverse, G[_]: Traverse]: Traverse[F <<< G] = Traverse[F].compose[G]
    given Traverse[IList] = strict.semiauto.traverse
    given Traverse[Snoc] = strict.semiauto.traverse
    given NonEmptyTraverse[ICons] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[SCons] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Tree] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelSCons] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelAndOne] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[VecAndNel] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Interleaved] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[EnumK1] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[AtLeastOne] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Singletons] = strict.semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Search] = strict.semiauto.nonEmptyTraverse

  object derivedInstances:
    case class ICons[A](x: ADTs.ICons[A]) derives NonEmptyTraverse
    case class Tree[A](x: ADTs.Tree[A]) derives NonEmptyTraverse
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives NonEmptyTraverse
    case class EnumK1[A](x: ADTs.EnumK1[A]) derives NonEmptyTraverse
    case class AtLeastOne[A](x: ADTs.AtLeastOne[A]) derives NonEmptyTraverse
    case class Singletons[A](x: ADTs.Singletons[A]) derives NonEmptyTraverse

end NonEmptyTraverseSuite
