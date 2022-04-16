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
import alleycats.std.all.*
import cats.data.NonEmptyList
import cats.laws.discipline.SerializableTests
import cats.syntax.all.*
import shapeless3.test.illTyped

import scala.compiletime.summonInline

class EmptyKSuite extends KittensSuite:
  import EmptyKSuite.*
  import TestDefns.*

  given Pure[Box] with
    def pure[A](a: A) = Box(a)

  inline def emptyK[F[_]] =
    summonInline[EmptyK[F]].empty

  inline def testEmptyK(context: String): Unit =
    test(s"$context.EmptyK[LOption]")(assert(emptyK[LOption] == Nil))
    test(s"$context.EmptyK[PList]")(assert(emptyK[PList] == (Nil, Nil)))
    test(s"$context.EmptyK[CaseClassWOption]")(assert(emptyK[CaseClassWOption] == CaseClassWOption(None)))
    test(s"$context.EmptyK[NelOption]")(assert(emptyK[NelOption] == NonEmptyList.one(None)))
    test(s"$context.EmptyK[IList]")(assert(emptyK[IList] == INil()))
    test(s"$context.EmptyK[Snoc]")(assert(emptyK[Snoc] == SNil()))
    test(s"$context.EmptyK respects existing instances")(assert(emptyK[BoxColor] == Box(Color(255, 255, 255))))
    checkAll(s"$context.EmptyK is Serializable", SerializableTests.serializable(summonInline[EmptyK[LOption]]))

  locally {
    import auto.emptyK.given
    testEmptyK("auto")
  }

  locally {
    import semiInstances.given
    testEmptyK("semiauto")
  }

end EmptyKSuite

object EmptyKSuite:
  import TestDefns.*

  type LOption[A] = List[Option[A]]
  type PList[A] = (List[A], List[A])
  type NelOption[A] = NonEmptyList[Option[A]]
  type BoxColor[A] = Box[Color[A]]

  object semiInstances:
    given EmptyK[LOption] = semiauto.emptyK
    given EmptyK[PList] = semiauto.emptyK
    given EmptyK[CaseClassWOption] = semiauto.emptyK
    given EmptyK[NelOption] = semiauto.emptyK
    given EmptyK[IList] = semiauto.emptyK
    given EmptyK[Snoc] = semiauto.emptyK
    given EmptyK[BoxColor] = semiauto.emptyK

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color:
    given EmptyK[Color] with
      def empty[A] = Color(255, 255, 255)

end EmptyKSuite
