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

import cats.Apply
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import scala.compiletime.*

class ApplySuite extends KittensSuite:
  import ApplySuite.*
  import TestDefns.*

  inline given [F[_]]: Isomorphisms[F] =
    Isomorphisms.invariant(summonInline[Apply[F]])

  inline def tests[F[_]]: ApplyTests[F] =
    ApplyTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].apply[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].apply[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].apply[Int, String, Long])
    checkAll(s"$instance[ListBox]", tests[ListBox].apply[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Apply[Interleaved]]))

  locally {
    import auto.apply.given
    validate("auto.apply")
  }

  locally {
    import semiInstances.given
    validate("semiauto.apply")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.apply"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].apply[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].apply[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Apply[Interleaved]]))
  }

end ApplySuite

object ApplySuite:
  import TestDefns.*

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

  object derivedInstances:
    case class CaseClassWOption[A](x: TestDefns.CaseClassWOption[A]) derives Apply
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Apply
    case class AndInt[A](x: ApplySuite.AndInt[A]) derives Apply

end ApplySuite