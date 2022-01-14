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

import cats.{Eq, NonEmptyTraverse}
import cats.data.{NonEmptyList, NonEmptyVector, OneAnd}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.{NonEmptyTraverseTests, SerializableTests}
import cats.syntax.all.*
import org.scalacheck.Arbitrary

import scala.compiletime.*

class NonEmptyTraverseSuite extends KittensSuite:
  import NonEmptyTraverseSuite.*
  import TestDefns.*

  inline def nonEmptyTraverseTests[F[_]]: NonEmptyTraverseTests[F] =
    NonEmptyTraverseTests[F](summonInline)

  inline def testReducible(inline context: String): Unit =
    checkAll(
      s"$context.NonEmptyTraverse[ICons]",
      nonEmptyTraverseTests[ICons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$context.NonEmptyTraverse[Tree]",
      nonEmptyTraverseTests[Tree].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    // FIXME: Those don't work
//    checkAll(
//      s"$context.NonEmptyTraverse[NelSCons]",
//      nonEmptyTraverseTests[NelSCons].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
//    )
//    checkAll(
//      s"$context.NonEmptyTraverse[NelAndOne]",
//      nonEmptyTraverseTests[NelAndOne].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
//    )
    checkAll(
      s"$context.NonEmptyTraverse[VecAndNel]",
      nonEmptyTraverseTests[VecAndNel].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$context.NonEmptyTraverse[Interleaved]",
      nonEmptyTraverseTests[Interleaved].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll(
      s"$context.NonEmptyTraverse is Serializable",
      SerializableTests.serializable(summonInline[NonEmptyTraverse[Tree]])
    )

  locally {
    import auto.nonEmptyTraverse.given
    testReducible("auto")
  }

  locally {
    import semiInstances.given
    testReducible("semiauto")
  }

end NonEmptyTraverseSuite

object NonEmptyTraverseSuite:
  import TestDefns.*

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[List, A]]

  // FIXME: Doesn't work if we define `ListAndNel` as a type alias
  final case class VecAndNel[A](vec: Vector[A], nel: NonEmptyList[A])
  object VecAndNel:
    given [A: Eq]: Eq[VecAndNel[A]] =
      (x, y) => x.vec === y.vec && x.nel === y.nel

    given [A: Arbitrary]: Arbitrary[VecAndNel[A]] =
      Arbitrary(for
        vec <- Arbitrary.arbitrary[Vector[A]]
        nel <- Arbitrary.arbitrary[NonEmptyList[A]]
      yield VecAndNel(vec, nel))

  object semiInstances:
    given NonEmptyTraverse[ICons] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Tree] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelSCons] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[NelAndOne] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[VecAndNel] = semiauto.nonEmptyTraverse
    given NonEmptyTraverse[Interleaved] = semiauto.nonEmptyTraverse

end NonEmptyTraverseSuite
