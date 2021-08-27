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
import org.scalacheck.Arbitrary

class ReducibleSuite extends KittensSuite {
  import ReducibleSuite.*
  import TestDefns.*

  def testReducible(context: String)(implicit
      iCons: Reducible[ICons],
      tree: Reducible[Tree],
      nelSCons: Reducible[NelSCons],
      nelAndOne: Reducible[NelAndOne],
      listAndNel: Reducible[ListAndNel],
      interleaved: Reducible[Interleaved],
      boxZipper: Reducible[BoxZipper]
  ): Unit = {
    checkAll(s"$context.Reducible[ICons]", ReducibleTests[ICons].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[Tree]", ReducibleTests[Tree].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[NelSCons]", ReducibleTests[NelSCons].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[NelAndOne]", ReducibleTests[NelAndOne].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[ListAndNel]", ReducibleTests[ListAndNel].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[Interleaved]", ReducibleTests[Interleaved].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible[BoxZipper]", ReducibleTests[BoxZipper].reducible[Option, Int, Long])
    checkAll(s"$context.Reducible is Serializable", SerializableTests.serializable(Reducible[Tree]))
  }

  locally {
    import auto.reducible.given
    testReducible("auto")
  }

  locally {
    import semiInstances._
    testReducible("semiauto")
  }
}

object ReducibleSuite {
  import TestDefns.*

  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[List, A]]
  type ListAndNel[A] = (List[A], NonEmptyList[A])
  type BoxZipper[A] = Box[Zipper[A]]

  object semiInstances {
    implicit val iCons: Reducible[ICons] = semiauto.reducible
    implicit val tree: Reducible[Tree] = semiauto.reducible
    implicit val nelSCons: Reducible[NelSCons] = semiauto.reducible
    implicit val nelAndOne: Reducible[NelAndOne] = semiauto.reducible
    implicit val listAndNel: Reducible[ListAndNel] = semiauto.reducible
    implicit val interleaved: Reducible[Interleaved] = semiauto.reducible
    implicit val boxZipper: Reducible[BoxZipper] = semiauto.reducible
  }

  final case class Zipper[+A](left: List[A], focus: A, right: List[A])
  object Zipper {

    implicit def eqv[A](implicit A: Eq[A]): Eq[Zipper[A]] = {
      val listEq = Eq[List[A]]
      Eq.instance { (x, y) =>
        A.eqv(x.focus, y.focus) && listEq.eqv(x.left, y.left) && listEq.eqv(x.right, y.right)
      }
    }

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Zipper[A]] =
      Arbitrary(for {
        left <- Arbitrary.arbitrary[List[A]]
        focus <- Arbitrary.arbitrary[A]
        right <- Arbitrary.arbitrary[List[A]]
      } yield Zipper(left, focus, right))

    implicit val reducible: Reducible[Zipper] = new Reducible[Zipper] {

      def reduceLeftTo[A, B](fa: Zipper[A])(f: A => B)(g: (B, A) => B) =
        Reducible[NonEmptyList].reduceLeftTo(NonEmptyList(fa.focus, fa.right))(f)(g)

      def reduceRightTo[A, B](fa: Zipper[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]) =
        Reducible[NonEmptyList].reduceRightTo(NonEmptyList(fa.focus, fa.right))(f)(g)

      def foldLeft[A, B](fa: Zipper[A], b: B)(f: (B, A) => B) =
        Foldable[List].foldLeft(fa.focus :: fa.right, b)(f)

      def foldRight[A, B](fa: Zipper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        Foldable[List].foldRight(fa.focus :: fa.right, lb)(f)
    }
  }
}
