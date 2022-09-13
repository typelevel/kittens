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

import cats.Applicative
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Arbitrary

import scala.compiletime.*

class ApplicativeSuite extends KittensSuite:
  import ApplicativeSuite.*
  import TestDefns.*

  inline given [F[_]]: Isomorphisms[F] =
    Isomorphisms.invariant(summonInline[Applicative[F]])

  inline def tests[F[_]]: ApplicativeTests[F] =
    ApplicativeTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].applicative[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].applicative[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].applicative[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$instance[ListBox]", tests[ListBox].applicative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Applicative[Interleaved]]))

  locally {
    import auto.applicative.given
    validate("auto.applicative")
  }

  locally {
    import semiApplicative.given
    validate("semiauto.applicative")
  }

  locally {
    import derivedApplicative.*
    val instance = "derived.applicative"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].applicative[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].applicative[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Applicative[Interleaved]]))
  }

end ApplicativeSuite

object ApplicativeSuite:
  import TestDefns.*

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiApplicative:
    given Applicative[Box] = semiauto.applicative
    given Applicative[CaseClassWOption] = semiauto.applicative
    given Applicative[OptList] = semiauto.applicative
    given Applicative[AndInt] = semiauto.applicative
    given Applicative[Interleaved] = semiauto.applicative
    given Applicative[ListBox] = semiauto.applicative

  object derivedApplicative:
    case class CaseClassWOption[A](x: TestDefns.CaseClassWOption[A]) derives Applicative
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Applicative
    case class AndInt[A](x: ApplicativeSuite.AndInt[A]) derives Applicative

end ApplicativeSuite
