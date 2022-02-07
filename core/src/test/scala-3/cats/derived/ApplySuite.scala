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
import cats.laws.discipline.{ApplyTests, SerializableTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms

class ApplySuite extends KittensSuite {
  import ApplySuite._
  import TestDefns._

  def testApply(context: String)(using
      caseClassWOption: Apply[CaseClassWOption],
      optList: Apply[OptList],
      // Requires scala 3.1.2
//      andInt: Apply[AndInt],
      interleaved: Apply[Interleaved],
      listBox: Apply[ListBox]
  ): Unit = {
    given isoOptList: Isomorphisms[OptList] = Isomorphisms.invariant(optList)
//    given isoAndInt: Isomorphisms[AndInt] = Isomorphisms.invariant(andInt)
    given isoListBox: Isomorphisms[ListBox] = Isomorphisms.invariant(listBox)

    checkAll(s"$context.Apply[CaseClassWOption]", ApplyTests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$context.Apply[OptList]", ApplyTests[OptList].apply[Int, String, Long])
//    checkAll(s"$context.Apply[AndInt]", ApplyTests[AndInt].apply[Int, String, Long])
    checkAll(s"$context.Apply[Interleaved]", ApplyTests[Interleaved].apply[Int, String, Long])
    checkAll(s"$context.Apply[ListBox]", ApplyTests[ListBox].apply[Int, String, Long])
    checkAll(s"$context.Apply is Serializable", SerializableTests.serializable(Apply[Interleaved]))
  }

  {
    import auto.apply.given
    testApply("auto")
  }

  {
    import semiInstances.given
    testApply("semiauto")
  }
}

object ApplySuite {
  import TestDefns._

  type OptList[A] = Option[List[A]]
//  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiInstances {
    given Apply[Box] = semiauto.apply
    given Apply[CaseClassWOption] = semiauto.apply
    given Apply[OptList] = semiauto.apply
//    given Apply[AndInt] = semiauto.apply
    given Apply[Interleaved] = semiauto.apply
    given Apply[ListBox] = semiauto.apply
  }
}
