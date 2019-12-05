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

import cats.instances.all._
import cats.laws.discipline.{FoldableTests, SerializableTests}
import org.scalacheck.Arbitrary

class FoldableSuite extends KittensSuite {
  import FoldableSuite._
  import TestDefns._
  import TestEqInstances._

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type BoxNel[A] = Box[Nel[A]]

  def testFoldable(context: String)(
    implicit iList: Foldable[IList],
    tree: Foldable[Tree],
    genericAdt: Foldable[GenericAdt],
    optList: Foldable[OptList],
    listSnoc: Foldable[ListSnoc],
    andChar: Foldable[AndChar],
    interleaved: Foldable[Interleaved],
    boxNel: Foldable[BoxNel]
  ): Unit = {
    checkAll(s"$context.Foldable[IList]", FoldableTests[IList].foldable[Int, Long])
    checkAll(s"$context.Foldable[Tree]", FoldableTests[Tree].foldable[Int, Long])
    checkAll(s"$context.Foldable[GenericAdt]", FoldableTests[GenericAdt].foldable[Int, Long])
    checkAll(s"$context.Foldable[OptList]", FoldableTests[OptList].foldable[Int, Long])
    checkAll(s"$context.Foldable[ListSnoc]", FoldableTests[ListSnoc].foldable[Int, Long])
    checkAll(s"$context.Foldable[AndChar]", FoldableTests[AndChar].foldable[Int, Long])
    checkAll(s"$context.Foldable[Interleaved]", FoldableTests[Interleaved].foldable[Int, Long])
    checkAll(s"$context.Foldable[BoxNel]", FoldableTests[BoxNel].foldable[Int, Long])
    checkAll(s"$context.Foldable is Serializable", SerializableTests.serializable(Foldable[Tree]))

    val n = 10000
    val largeIList = IList.fromSeq(1 until n)
    val largeSnoc = Snoc.fromSeq(1 until n) :: Nil

    test(s"$context.Foldable.foldLeft is stack safe") {
      val actualIList = largeIList.foldLeft(0)(_ + _)
      val actualSnoc = listSnoc.foldLeft(largeSnoc, 0)(_ + _)
      val expected = n * (n - 1) / 2
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }

    test(s"$context.Foldable.foldRight is stack safe") {
      val actualIList = largeIList.foldRight(Eval.Zero)((i, sum) => sum.map(_ + i))
      val actualSnoc = listSnoc.foldRight(largeSnoc, Eval.Zero)((i, sum) => sum.map(_ + i))
      val expected = n * (n - 1) / 2
      assert(actualIList.value == expected)
      assert(actualSnoc.value == expected)
    }

    test(s"$context.Foldable respects existing instances") {
      val tail = List.range(1, 10)
      val sum = boxNel.fold(Box(Nel(42, tail)))
      assert(sum == tail.sum)
    }
  }

  {
    import auto.foldable._
    testFoldable("auto")
  }

  {
    import cached.foldable._
    testFoldable("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val iList: Foldable[IList] = semi.foldable
    implicit val tree: Foldable[Tree] = semi.foldable
    implicit val genericAdt: Foldable[GenericAdt] = semi.foldable
    implicit val optList: Foldable[OptList] = semi.foldable
    implicit val listSnoc: Foldable[ListSnoc] = semi.foldable
    implicit val andChar: Foldable[AndChar] = semi.foldable
    implicit val interleaved: Foldable[Interleaved] = semi.foldable
    implicit val boxNel: Foldable[BoxNel] = semi.foldable
    def run(): Unit = testFoldable("semi")
  }
}

object FoldableSuite {

  final case class Nel[+A](head: A, tail: List[A])
  object Nel {

    implicit def eqv[A](implicit A: Eq[A]): Eq[Nel[A]] = {
      val listEq = Eq[List[A]]
      Eq.instance((x, y) => A.eqv(x.head, y.head) && listEq.eqv(x.tail, y.tail))
    }

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Nel[A]] =
      Arbitrary(for {
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[List[A]]
      } yield Nel(head, tail))

    implicit val foldable: Foldable[Nel] = new Foldable[Nel] {

      def foldLeft[A, B](fa: Nel[A], b: B)(f: (B, A) => B) =
        Foldable[List].foldLeft(fa.tail, b)(f)

      def foldRight[A, B](fa: Nel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
        Foldable[List].foldRight(fa.tail, lb)(f)
    }
  }
}
