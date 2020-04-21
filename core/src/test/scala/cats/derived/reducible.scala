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
import cats.instances.all._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ReducibleTests, SerializableTests}
import org.scalacheck.Arbitrary

class ReducibleSuite extends KittensSuite {
  import ReducibleSuite._
  import TestDefns._
  import TestEqInstances._

  def testReducible(context: String)(
    implicit iCons: Reducible[ICons],
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

    val n = 10000
    val largeIList = ICons(0, IList.fromSeq(1 until n))
    val largeSnoc = NonEmptyList.one(SCons(Snoc.fromSeq(1 until n), 0))

    test(s"$context.Reducible.reduceLeft is stack safe") {
      val actualIList = largeIList.reduceLeft(_ + _)
      val actualSnoc = nelSCons.reduceLeft(largeSnoc)(_ + _)
      val expected = n * (n - 1) / 2
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }

    test(s"$context.Reducible.reduceRight is stack safe") {
      val actualIList = largeIList.reduceRight((i, sum) => sum.map(_ + i))
      val actualSnoc = nelSCons.reduceRight(largeSnoc)((i, sum) => sum.map(_ + i))
      val expected = n * (n - 1) / 2
      assert(actualIList.value == expected)
      assert(actualSnoc.value == expected)
    }

    test(s"$context.Reducible respects existing instances") {
      val list = List.range(1, 10)
      val sum = boxZipper.reduce(Box(Zipper(list, 42, list)))
      assert(sum == list.sum + 42)
    }
  }

  {
    import auto.reducible._
    testReducible("auto")
  }

  {
    import cached.reducible._
    testReducible("cached")
  }

  {
    import semiInstances._
    testReducible("semiauto")
  }
}

object ReducibleSuite {
  import TestDefns._

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
