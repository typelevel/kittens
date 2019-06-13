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
import cats.laws.discipline.ApplyTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms

class ApplySuite extends KittensSuite {
  import TestDefns._
  import TestEqInstances._

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  def testApply(context: String)(
    implicit caseClassWOption: Apply[CaseClassWOption],
    optList: Apply[OptList],
    andInt: Apply[AndInt],
    interleaved: Apply[Interleaved],
    listBox: Apply[ListBox]
  ): Unit = {
    implicit val isoOptList: Isomorphisms[OptList] = Isomorphisms.invariant(optList)
    implicit val isoAndInt: Isomorphisms[AndInt] = Isomorphisms.invariant(andInt)
    implicit val isoListBox: Isomorphisms[ListBox] = Isomorphisms.invariant(listBox)
    checkAll(s"$context.Apply[CaseClassWOption]", ApplyTests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$context.Apply[OptList]", ApplyTests[OptList].apply[Int, String, Long])
    checkAll(s"$context.Apply[AndInt]", ApplyTests[AndInt].apply[Int, String, Long])
    checkAll(s"$context.Apply[Interleaved]", ApplyTests[Interleaved].apply[Int, String, Long])
    checkAll(s"$context.Apply[ListBox]", ApplyTests[ListBox].apply[Int, String, Long])
  }

  {
    import auto.apply._
    testApply("auto")
  }

  {
    import cached.apply._
    testApply("cached")
  }

  {
    implicit val caseClassWOption: Apply[CaseClassWOption] = semi.apply
    implicit val optList: Apply[OptList] = semi.apply
    implicit val andInt: Apply[AndInt] = semi.apply
    implicit val interleaved: Apply[Interleaved] = semi.apply
    implicit val listBox: Apply[ListBox] = semi.apply
    testApply("semi")
  }
}

