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

import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.{ReducibleTests, SerializableTests}
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import scala.compiletime.*

class ReducibleSuite extends KittensSuite:
  import ReducibleSuite.*
  import TestDefns.*

  inline def reducibleTests[F[_]]: ReducibleTests[F] =
    ReducibleTests[F](summonInline)

  inline def testReducible(context: String): Unit =
    checkAll(s"$context.Reducible[ICons]", reducibleTests[ICons].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[Tree]", reducibleTests[Tree].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[NelSCons]", reducibleTests[NelSCons].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[NelAndOne]", reducibleTests[NelAndOne].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[VecAndNel]", reducibleTests[VecAndNel].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[Interleaved]", reducibleTests[Interleaved].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[BoxZipper]", reducibleTests[BoxZipper].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[EnumK1]", reducibleTests[EnumK1].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible is Serializable", SerializableTests.serializable(summonInline[Reducible[Tree]]))

  locally {
    import auto.reducible.given
    testReducible("auto")
  }

  locally {
    import semiInstances.given
    testReducible("semiauto")
  }

end ReducibleSuite

object ReducibleSuite:
  import TestDefns.*

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[Vector, A]]
  type VecAndNel[A] = (Vector[A], NonEmptyList[A])
  type BoxZipper[A] = Box[Zipper[A]]

  object semiInstances:
    given Reducible[ICons] = semiauto.reducible
    given Reducible[Tree] = semiauto.reducible
    given Reducible[NelSCons] = semiauto.reducible
    given Reducible[NelAndOne] = semiauto.reducible
    given Reducible[VecAndNel] = semiauto.reducible
    given Reducible[Interleaved] = semiauto.reducible
    given Reducible[BoxZipper] = semiauto.reducible
    given Reducible[EnumK1] = semiauto.reducible

  final case class Zipper[+A](left: List[A], focus: A, right: List[A])
  object Zipper:
    given [A: Eq]: Eq[Zipper[A]] =
      (x, y) => x.focus === y.focus && x.left === y.left && x.right === y.right

    given [A: Arbitrary]: Arbitrary[Zipper[A]] =
      Arbitrary(for
        left <- Arbitrary.arbitrary[List[A]]
        focus <- Arbitrary.arbitrary[A]
        right <- Arbitrary.arbitrary[List[A]]
      yield Zipper(left, focus, right))

    given Reducible[Zipper] with
      def reduceLeftTo[A, B](fa: Zipper[A])(f: A => B)(g: (B, A) => B) =
        NonEmptyList(fa.focus, fa.right).reduceLeftTo(f)(g)
      def reduceRightTo[A, B](fa: Zipper[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        NonEmptyList(fa.focus, fa.right).reduceRightTo(f)(g)
      def foldLeft[A, B](fa: Zipper[A], b: B)(f: (B, A) => B) =
        (fa.focus :: fa.right).foldl(b)(f)
      def foldRight[A, B](fa: Zipper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        (fa.focus :: fa.right).foldr(lb)(f)

end ReducibleSuite
