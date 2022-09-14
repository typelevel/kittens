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

import scala.compiletime.*

class ApplicativeSuite extends KittensSuite:
  import ApplicativeSuite.*
  import TestDefns.*

  inline def applicativeTests[F[_]](f: Isomorphisms[F] ?=> (at: ApplicativeTests[F]) => at.RuleSet) =
    f(using summonInline)(ApplicativeTests[F](summonInline))

  inline def testApplicative(inline context: String): Unit =
    checkAll(
      s"$context.Applicative[CaseClassWOption]",
      applicativeTests[CaseClassWOption](_.applicative[Int, String, Long])
    )
    checkAll(s"$context.Applicative[OptList]", applicativeTests[OptList](_.applicative[Int, String, Long]))
    checkAll(s"$context.Applicative[AndInt]", applicativeTests[AndInt](_.applicative[Int, String, Long]))
    checkAll(s"$context.Applicative[Interleaved]", applicativeTests[Interleaved](_.applicative[Int, String, Long]))
    checkAll(s"$context.Applicative[ListBox]", applicativeTests[ListBox](_.applicative[Int, String, Long]))
    checkAll(
      s"$context.Applicative is Serializable",
      SerializableTests.serializable(summonInline[Applicative[Interleaved]])
    )

  locally {
    import auto.applicative.given
    testApplicative("auto")
  }

  locally {
    import semiInstances.given
    testApplicative("semiauto")
  }

end ApplicativeSuite

object ApplicativeSuite:
  import TestDefns.*

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiInstances:
    given Applicative[Box] = semiauto.applicative
    given Applicative[CaseClassWOption] = semiauto.applicative
    given Applicative[OptList] = semiauto.applicative
    given Applicative[AndInt] = semiauto.applicative
    given Applicative[Interleaved] = semiauto.applicative
    given Applicative[ListBox] = semiauto.applicative

end ApplicativeSuite
