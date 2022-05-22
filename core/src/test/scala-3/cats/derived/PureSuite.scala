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

import alleycats.Pure
import cats.data.NonEmptyList
import cats.laws.discipline.SerializableTests
import shapeless3.test.illTyped

import scala.compiletime.summonInline

class PureSuite extends KittensSuite:
  import PureSuite.*
  import TestDefns.*

  extension [A](a: A)
    inline def pure[F[_]] =
      summonInline[Pure[F]].pure(a)

  inline def testPure(inline context: String): Unit =
    test(s"$context.Pure[LOption]")(assert(42.pure[LOption] == Some(42) :: Nil))
    test(s"$context.Pure[PList]")(assert("Scala".pure[PList] == ("Scala" :: Nil, "Scala" :: Nil)))
    test(s"$context.Pure[CaseClassWOption]")(assert(3.14.pure[CaseClassWOption] == CaseClassWOption(Some(3.14))))
    test(s"$context.Pure[NelOption]")(assert(42.pure[NelOption] == NonEmptyList.of(Some(42))))
    test(s"$context.Pure[Interleaved]")(assert('x'.pure[Interleaved] == Interleaved(0, 'x', 0, Vector('x'), "")))
    test(s"$context.Pure respects existing instances")(assert(().pure[BoxColor] == Box(Color(255, 255, 255))))
    checkAll(s"$context.Pure is Serializable", SerializableTests.serializable(summonInline[Pure[Interleaved]]))

  locally {
    import auto.pure.given
    testPure("auto")
    illTyped("Pure[IList]")
    illTyped("Pure[Snoc]")
  }

  locally {
    import semiInstances.given
    testPure("semiauto")
    illTyped("semiauto.pure[IList]")
    illTyped("semiauto.pure[Snoc]")
  }

end PureSuite

object PureSuite:
  import TestDefns.*

  type LOption[A] = List[Option[A]]
  type PList[A] = (List[A], List[A])
  type NelOption[A] = NonEmptyList[Option[A]]
  type BoxColor[A] = Box[Color[A]]

  object semiInstances:
    given Pure[LOption] = semiauto.pure
    given Pure[PList] = semiauto.pure
    given Pure[CaseClassWOption] = semiauto.pure
    given Pure[NelOption] = semiauto.pure
    given Pure[Interleaved] = semiauto.pure
    given Pure[BoxColor] = semiauto.pure

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color:
    given Pure[Color] with
      def pure[A](value: A) = Color(255, 255, 255)

end PureSuite
