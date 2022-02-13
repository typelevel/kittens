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
import cats.laws.discipline.{ApplicativeTests, SerializableTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms

class ApplicativeSuite extends KittensSuite {
  import ApplicativeSuite._
  import TestDefns._

  def testApplicative(context: String)(using
      caseClassWOption: Applicative[CaseClassWOption],
      optList: Applicative[OptList],
      // Requires Dotty 3.1.2
//      andInt: Applicative[AndInt],
      interleaved: Applicative[Interleaved],
      listBox: Applicative[ListBox]
  ): Unit = {
    checkAll(
      s"$context.Applicative[CaseClassWOption]",
      ApplicativeTests[CaseClassWOption].applicative[Int, String, Long]
    )
    checkAll(s"$context.Applicative[OptList]", ApplicativeTests[OptList].applicative[Int, String, Long])
//    checkAll(s"$context.Applicative[AndInt]", ApplicativeTests[AndInt].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[Interleaved]", ApplicativeTests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[ListBox]", ApplicativeTests[ListBox].applicative[Int, String, Long])
    checkAll(s"$context.Applicative is Serializable", SerializableTests.serializable(Applicative[Interleaved]))
  }

  {
    import auto.applicative.given
    testApplicative("auto")
  }

  {
    import semiInstances.given
    testApplicative("semiauto")
  }
}

object ApplicativeSuite {
  import TestDefns._
  import cats.instances.tuple._

  type OptList[A] = Option[List[A]]
//  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiInstances {
    given Applicative[Box] = semiauto.applicative
    given Applicative[CaseClassWOption] = semiauto.applicative
    given Applicative[OptList] = semiauto.applicative
//    given Applicative[AndInt] = semiauto.applicative
    given Applicative[Interleaved] = semiauto.applicative
    given Applicative[ListBox] = semiauto.applicative
  }
}
