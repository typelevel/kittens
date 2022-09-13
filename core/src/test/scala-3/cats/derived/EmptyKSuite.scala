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
import scala.compiletime.summonInline

class EmptyKSuite extends KittensSuite:
  import EmptyKSuite.*
  import ADTs.*

  given Pure[Box] with
    def pure[A](a: A) = Box(a)

  inline def emptyK[F[_]] =
    summonInline[EmptyK[F]].empty

  inline def validate(instance: String): Unit =
    test(s"$instance[LOption]")(assert(emptyK[LOption] == Nil))
    test(s"$instance[PList]")(assert(emptyK[PList] == (Nil, Nil)))
    test(s"$instance[CaseClassWOption]")(assert(emptyK[CaseClassWOption] == CaseClassWOption(None)))
    test(s"$instance[NelOption]")(assert(emptyK[NelOption] == NonEmptyList.one(None)))
    test(s"$instance[IList]")(assert(emptyK[IList] == INil()))
    test(s"$instance[Snoc]")(assert(emptyK[Snoc] == SNil()))
    test(s"$instance respects existing instances")(assert(emptyK[BoxColor] == Box(Color(255, 255, 255))))
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[EmptyK[LOption]]))

  locally {
    import auto.emptyK.given
    validate("auto.emptyK")
  }

  locally {
    import semiInstances.given
    validate("semiauto.emptyK")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.emptyK"
    test(s"$instance[CaseClassWOption]")(assert(emptyK[CaseClassWOption].x.value.isEmpty))
    test(s"$instance[IList]")(assert(emptyK[IList].x == INil()))
    test(s"$instance[Snoc]")(assert(emptyK[Snoc].x == SNil()))
    checkAll(s"$instance is Serializable", SerializableTests.serializable(EmptyK[Snoc]))
  }

end EmptyKSuite

object EmptyKSuite:
  import ADTs.*

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

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives EmptyK
    case class IList[A](x: ADTs.IList[A]) derives EmptyK
    case class Snoc[A](x: ADTs.Snoc[A]) derives EmptyK

  final case class Color[A](r: Int, g: Int, b: Int)
  object Color:
    given EmptyK[Color] with
      def empty[A] = Color(255, 255, 255)

end EmptyKSuite
