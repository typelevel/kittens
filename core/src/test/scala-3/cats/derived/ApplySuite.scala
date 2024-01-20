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

import cats.laws.discipline.*
import cats.{Apply, Semigroup}
import shapeless3.deriving.Const

import scala.compiletime.*

class ApplySuite extends KittensSuite:
  import ADTs.*
  import ApplySuite.*

  inline def tests[F[_]]: ApplyTests[F] =
    ApplyTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].apply[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].apply[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].apply[Int, String, Long])
    checkAll(s"$instance[ListBox]", tests[ListBox].apply[Int, String, Long])
    checkAll(s"$instance[Search]", tests[Search].apply[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Apply[Interleaved]]))

  locally:
    import auto.apply.given
    validate("auto.apply")

  locally:
    import semiInstances.given
    validate("semiauto.apply")

  locally:
    import semiInstances.given
    validate("strict.semiauto.apply")
    testNoInstance("strict.semiauto.apply", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.apply"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].apply[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].apply[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Apply[Interleaved]))

end ApplySuite

object ApplySuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiInstances:
    given Apply[Box] = semiauto.apply
    given Apply[CaseClassWOption] = semiauto.apply
    given Apply[OptList] = semiauto.apply
    given Apply[AndInt] = semiauto.apply
    given Apply[Interleaved] = semiauto.apply
    given Apply[ListBox] = semiauto.apply
    given Apply[Search] = semiauto.apply

  object strictInstances:
    given [T: Semigroup]: Apply[Const[T]] = semiauto.apply
    given [F[_]: Apply, G[_]: Apply]: Apply[[x] =>> F[G[x]]] = Apply[F].compose[G]
    given Apply[Box] = strict.semiauto.apply
    given Apply[CaseClassWOption] = strict.semiauto.apply
    given Apply[AndInt] = strict.semiauto.apply
    given Apply[Interleaved] = strict.semiauto.apply
    given Apply[Search] = strict.semiauto.apply

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives Apply
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Apply
    case class AndInt[A](x: ApplySuite.AndInt[A]) derives Apply

end ApplySuite
