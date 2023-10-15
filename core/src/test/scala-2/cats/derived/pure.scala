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

import alleycats.Pure
import cats.data.NonEmptyList
import cats.laws.discipline.SerializableTests
import shapeless.test.illTyped

class PureSuite extends KittensSuite {
  import PureSuite._
  import TestDefns._

  def testPure(context: String)(implicit
      lOption: Pure[LOption],
      pList: Pure[PList],
      caseClassWOption: Pure[CaseClassWOption],
      nelOption: Pure[NelOption],
      interleaved: Pure[Interleaved],
      singletons: Pure[Singletons],
      boxColor: Pure[BoxColor]
  ): Unit = {
    test(s"$context.Pure[LOption]")(assert(lOption.pure(42) == Some(42) :: Nil))
    test(s"$context.Pure[PList]")(assert(pList.pure("Scala") == ("Scala" :: Nil, "Scala" :: Nil)))
    test(s"$context.Pure[CaseClassWOption]")(assert(caseClassWOption.pure(3.14) == CaseClassWOption(Some(3.14))))
    test(s"$context.Pure[NelOption]")(assert(nelOption.pure(42) == NonEmptyList.of(Some(42))))
    test(s"$context.Pure[Interleaved]")(assert(interleaved.pure('x') == Interleaved(0, 'x', 0, 'x' :: Nil, "")))
    test(s"$context.Pure[Singletons]")(assert(singletons.pure('x') == Singletons.wrap('x')))
    test(s"$context.Pure respects existing instances")(assert(boxColor.pure(()) == Box(Color(255, 255, 255))))
    checkAll(s"$context.Pure is Serializable", SerializableTests.serializable(Pure[Interleaved]))
  }

  {
    import auto.pure._
    testPure("auto")
    illTyped("Pure[IList]")
    illTyped("Pure[Snoc]")
  }

  {
    import cached.pure._
    testPure("cached")
    illTyped("Pure[IList]")
    illTyped("Pure[Snoc]")
  }

  {
    import semiInstances._
    testPure("semiauto")
    illTyped("semiauto.pure[IList]")
    illTyped("semiauto.pure[Snoc]")
  }
}

object PureSuite {
  import TestDefns._

  type LOption[A] = List[Option[A]]
  type PList[A] = (List[A], List[A])
  type NelOption[A] = NonEmptyList[Option[A]]
  type BoxColor[A] = Box[Color[A]]

  object semiInstances {
    implicit val lOption: Pure[LOption] = semiauto.pure
    implicit val pList: Pure[PList] = semiauto.pure
    implicit val caseClassWOption: Pure[CaseClassWOption] = semiauto.pure
    implicit val nelOption: Pure[NelOption] = semiauto.pure
    implicit val interleaved: Pure[Interleaved] = semiauto.pure
    implicit val singletons: Pure[Singletons] = semiauto.pure
    implicit val boxColor: Pure[BoxColor] = semiauto.pure
  }

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color {

    implicit val pure: Pure[Color] = new Pure[Color] {
      def pure[A](value: A) = Color(255, 255, 255)
    }
  }
}
