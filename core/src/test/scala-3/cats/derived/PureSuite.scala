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

import alleycats.{Empty, Pure}
import cats.data.NonEmptyList
import cats.laws.discipline.SerializableTests
import shapeless3.deriving.Const

import scala.compiletime.*

class PureSuite extends KittensSuite:
  import PureSuite.*
  import ADTs.*

  extension [A](a: A)
    inline def pure[F[_]] =
      summonInline[Pure[F]].pure(a)

  inline def validate(inline instance: String): Unit =
    test(s"$instance[LOption]")(assert(42.pure[LOption] == Some(42) :: Nil))
    test(s"$instance[PList]")(assert("Scala".pure[PList] == ("Scala" :: Nil, "Scala" :: Nil)))
    test(s"$instance[CaseClassWOption]")(assert(3.14.pure[CaseClassWOption] == CaseClassWOption(Some(3.14))))
    test(s"$instance[NelOption]")(assert(42.pure[NelOption] == NonEmptyList.of(Some(42))))
    test(s"$instance[Interleaved]")(assert('x'.pure[Interleaved] == Interleaved(0, 'x', 0, Vector('x'), "")))
    test(s"$instance[Singletons]")(assert('x'.pure[Singletons] == Singletons('x')))
    test(s"$instance respects existing instances")(assert(().pure[BoxColor] == Box(Color(255, 255, 255))))
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Pure[Interleaved]]))

  locally:
    import auto.pure.given
    validate("auto.pure")
    testNoInstance("Pure", "IList")
    testNoInstance("Pure", "Snoc")

  locally:
    import semiInstances.given
    validate("semiauto.pure")
    testNoInstance("semiauto.pure", "IList")
    testNoInstance("semiauto.pure", "Snoc")

  locally:
    import strictInstances.given
    validate("strict.semiauto.pure")
    testNoInstance("strict.semiauto.pure", "IList")
    testNoInstance("strict.semiauto.pure", "Snoc")
    testNoInstance("strict.semiauto.pure", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.pure"
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Pure[Interleaved]))
    test(s"$instance[CaseClassWOption]")(assert(3.14.pure[CaseClassWOption].x == ADTs.CaseClassWOption(Some(3.14))))
    test(s"$instance[Interleaved]")(assert('x'.pure[Interleaved].x == ADTs.Interleaved(0, 'x', 0, Vector('x'), "")))

end PureSuite

object PureSuite:
  import ADTs.*

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
    given Pure[Singletons] = semiauto.pure
    given Pure[BoxColor] = semiauto.pure

  object strictInstances:
    given [T: Empty]: Pure[Const[T]] = semiauto.pure
    given [T <: Singleton: ValueOf]: Pure[Const[T]] = semiauto.pure
    given Pure[LOption] = semiauto.pure
    given Pure[PList] = strict.semiauto.pure
    given Pure[CaseClassWOption] = strict.semiauto.pure
    given Pure[NelOption] = semiauto.pure
    given Pure[Interleaved] = strict.semiauto.pure
    given Pure[Singletons] = strict.semiauto.pure
    given Pure[BoxColor] = strict.semiauto.pure

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives Pure
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Pure

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color:
    given Pure[Color] with
      def pure[A](value: A) = Color(255, 255, 255)

end PureSuite
