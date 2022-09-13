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

import cats.{Eval, Reducible}
import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.{ReducibleTests, SerializableTests}
import cats.syntax.all.*
import scala.compiletime.*

class ReducibleSuite extends KittensSuite:
  import ReducibleSuite.*
  import TestDefns.*

  inline def tests[F[_]]: ReducibleTests[F] =
    ReducibleTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[ICons]", tests[ICons].reducible[Option, Int, Long])
    checkAll(s"$instance[Tree]", tests[Tree].reducible[Option, Int, Long])
    checkAll(s"$instance[NelSCons]", tests[NelSCons].reducible[Option, Int, Long])
    checkAll(s"$instance[NelAndOne]", tests[NelAndOne].reducible[Option, Int, Long])
    checkAll(s"$instance[VecAndNel]", tests[VecAndNel].reducible[Option, Int, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].reducible[Option, Int, Long])
    checkAll(s"$instance[BoxZipper]", tests[BoxZipper].reducible[Option, Int, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].reducible[Option, Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Reducible[Tree]]))

  locally {
    import auto.reducible.given
    validate("auto.reducible")
  }

  locally {
    import semiReducible.given
    validate("semiauto.reducible")
  }

  locally {
    import derivedReducible.*
    val instance = "derived.reducible"
    checkAll(s"$instance[ICons]", tests[ICons].reducible[Option, Int, Long])
    checkAll(s"$instance[Tree]", tests[Tree].reducible[Option, Int, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].reducible[Option, Int, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].reducible[Option, Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Reducible[Tree]]))
  }

end ReducibleSuite

object ReducibleSuite:
  import TestDefns.*

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[Vector, A]]
  type VecAndNel[A] = (Vector[A], NonEmptyList[A])
  type BoxZipper[A] = Box[Zipper[A]]

  object semiReducible:
    given Reducible[ICons] = semiauto.reducible
    given Reducible[Tree] = semiauto.reducible
    given Reducible[NelSCons] = semiauto.reducible
    given Reducible[NelAndOne] = semiauto.reducible
    given Reducible[VecAndNel] = semiauto.reducible
    given Reducible[Interleaved] = semiauto.reducible
    given Reducible[BoxZipper] = semiauto.reducible
    given Reducible[EnumK1] = semiauto.reducible

  object derivedReducible:
    case class ICons[A](x: TestDefns.ICons[A]) derives Reducible
    case class Tree[A](x: TestDefns.Tree[A]) derives Reducible
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Reducible
    case class EnumK1[A](x: TestDefns.EnumK1[A]) derives Reducible

  final case class Zipper[+A](left: List[A], focus: A, right: List[A])
  object Zipper:
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
