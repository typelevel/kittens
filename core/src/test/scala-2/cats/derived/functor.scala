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
import cats.laws.discipline._
import cats.laws.discipline.eq._

class FunctorSuite extends KittensSuite {
  import FunctorSuite._
  import TestDefns._
  import TestEqInstances._

  implicit val exhaustivePred: ExhaustiveCheck[Predicate[Boolean]] =
    ExhaustiveCheck.instance(List(_ => true, _ => false, identity, !_))

  def testFunctor(context: String)(implicit
      iList: Functor[IList],
      tree: Functor[Tree],
      genericAdt: Functor[GenericAdt],
      optList: Functor[OptList],
      listSnoc: Functor[ListSnoc],
      andChar: Functor[AndChar],
      interleaved: Functor[Interleaved],
      nestedPred: Functor[NestedPred],
      singletons: Functor[Singletons]
  ): Unit = {
    checkAll(s"$context.Functor[IList]", FunctorTests[IList].functor[Int, String, Long])
    checkAll(s"$context.Functor[Tree]", FunctorTests[Tree].functor[Int, String, Long])
    checkAll(s"$context.Functor[GenericAdt]", FunctorTests[GenericAdt].functor[Int, String, Long])
    checkAll(s"$context.Functor[OptList]", FunctorTests[OptList].functor[Int, String, Long])
    checkAll(s"$context.Functor[ListSnoc]", FunctorTests[ListSnoc].functor[Int, String, Long])
    checkAll(s"$context.Functor[AndChar]", FunctorTests[AndChar].functor[Int, String, Long])
    checkAll(s"$context.Functor[Interleaved]", FunctorTests[Interleaved].functor[Int, String, Long])
    checkAll(s"$context.Functor[NestedPred]", FunctorTests[NestedPred].functor[Boolean, Int, Boolean])
    checkAll(s"$context.Functor[Singletons]", FunctorTests[Singletons].functor[Boolean, Int, Boolean])
    checkAll(s"$context.Functor is Serializable", SerializableTests.serializable(Functor[Tree]))

    test(s"$context.Functor.map is stack safe") {
      val n = 10000
      val largeIList = IList.fromSeq(1 until n)
      val largeSnoc = Snoc.fromSeq(1 until n) :: Nil
      val actualIList = IList.toList(largeIList.map(_ + 1))
      val actualSnoc = listSnoc.map(largeSnoc)(_ + 1).flatMap(Snoc.toList)
      val expected = (2 until n + 1).toList
      assert(actualIList == expected)
      assert(actualSnoc == expected)
    }
  }

  {
    import auto.functor._
    testFunctor("auto")
  }

  {
    import cached.functor._
    testFunctor("cached")
  }

  {
    import semiInstances._
    testFunctor("semiauto")
  }
}

object FunctorSuite {
  import TestDefns._

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type Predicate[A] = A => Boolean
  type NestedPred[A] = Predicate[Predicate[A]]

  object semiInstances {
    implicit val iList: Functor[IList] = semiauto.functor
    implicit val tree: Functor[Tree] = semiauto.functor
    implicit val genericAdt: Functor[GenericAdt] = semiauto.functor
    implicit val optList: Functor[OptList] = semiauto.functor
    implicit val listSnoc: Functor[ListSnoc] = semiauto.functor
    implicit val andChar: Functor[AndChar] = semiauto.functor
    implicit val interleaved: Functor[Interleaved] = semiauto.functor
    implicit val nestedPred: Functor[NestedPred] = semiauto.functor
    implicit val singletons: Functor[Singletons] = semiauto.functor
  }
}
