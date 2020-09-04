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

import alleycats.{EmptyK, Pure}
import alleycats.std.all._
import cats.data.NonEmptyList
import cats.laws.discipline.SerializableTests
import shapeless.test.illTyped

class EmptyKSuite extends KittensSuite {
  import EmptyKSuite._
  import TestDefns._

  def testEmptyK(context: String)(
    implicit lOption: EmptyK[LOption],
    pList: EmptyK[PList],
    caseClassWOption: EmptyK[CaseClassWOption],
    nelOption: EmptyK[NelOption],
    boxColor: EmptyK[BoxColor]
  ): Unit = {
    test(s"$context.EmptyK[LOption]")(assert(lOption.empty == Nil))
    test(s"$context.EmptyK[PList]")(assert(pList.empty == (Nil, Nil)))
    test(s"$context.EmptyK[CaseClassWOption]")(assert(caseClassWOption.empty == CaseClassWOption(None)))
    test(s"$context.EmptyK[NelOption]")(assert(nelOption.empty == NonEmptyList.of(None)))
    test(s"$context.EmptyK respects existing instances")(assert(boxColor.empty == Box(Color(255, 255, 255))))
    checkAll(s"$context.EmptyK is Serializable", SerializableTests.serializable(EmptyK[PList]))
  }

  implicit val pureBox: Pure[Box] = new Pure[Box] {
    def pure[A](a: A) = Box(a)
  }

  {
    import auto.emptyK._
    testEmptyK("auto")
    illTyped("EmptyK[IList]")
    illTyped("EmptyK[Snoc]")
  }

  {
    import cached.emptyK._
    testEmptyK("cached")
    illTyped("EmptyK[IList]")
    illTyped("EmptyK[Snoc]")
  }

  {
    import semiInstances._
    testEmptyK("semiauto")
    illTyped("semiauto.emptyK[IList]")
    illTyped("semiauto.emptyK[Snoc]")
  }
}

object EmptyKSuite {
  import TestDefns._

  type LOption[A] = List[Option[A]]
  type PList[A] = (List[A], List[A])
  type NelOption[A] = NonEmptyList[Option[A]]
  type BoxColor[A] = Box[Color[A]]

  object semiInstances {
    implicit val lOption: EmptyK[LOption] = semiauto.emptyK
    implicit val pList: EmptyK[PList] = semiauto.emptyK
    implicit val caseClassWOption: EmptyK[CaseClassWOption] = semiauto.emptyK
    implicit val nelOption: EmptyK[NelOption] = semiauto.emptyK
    implicit val boxColor: EmptyK[BoxColor] = semiauto.emptyK
  }

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color {

    implicit val emptyK: EmptyK[Color] = new EmptyK[Color] {
      def empty[A] = Color(255, 255, 255)
    }
  }
}
