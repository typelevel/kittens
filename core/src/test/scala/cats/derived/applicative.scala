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

package cats
package derived

import cats.instances.all._
import cats.laws.discipline.ApplicativeTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms

class ApplicativeSuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  def testApplicative(context: String)(
    implicit caseClassWOption: Applicative[CaseClassWOption],
    optList: Applicative[OptList],
    andInt: Applicative[AndInt],
    interleaved: Applicative[Interleaved],
    listBox: Applicative[ListBox]
  ): Unit = {
    implicit val isoOptList: Isomorphisms[OptList] = Isomorphisms.invariant(optList)
    implicit val isoAndInt: Isomorphisms[AndInt] = Isomorphisms.invariant(andInt)
    implicit val isoListBox: Isomorphisms[ListBox] = Isomorphisms.invariant(listBox)
    checkAll(s"$context.Applicative[CaseClassWOption]", ApplicativeTests[CaseClassWOption].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[OptList]", ApplicativeTests[OptList].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[AndInt]", ApplicativeTests[AndInt].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[Interleaved]", ApplicativeTests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$context.Applicative[ListBox]", ApplicativeTests[ListBox].applicative[Int, String, Long])
  }

  {
    import auto.applicative._
    testApplicative("auto")
  }

  {
    import cached.applicative._
    testApplicative("cached")
  }

  {
    implicit val caseClassWOption: Applicative[CaseClassWOption] = semi.applicative
    implicit val optList: Applicative[OptList] = semi.applicative
    implicit val andInt: Applicative[AndInt] = semi.applicative
    implicit val interleaved: Applicative[Interleaved] = semi.applicative
    implicit val listBox: Applicative[ListBox] = semi.applicative
    testApplicative("semi")
  }
}

