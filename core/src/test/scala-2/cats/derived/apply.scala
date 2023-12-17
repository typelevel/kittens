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

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.{ApplyTests, SerializableTests}
import shapeless._

class ApplySuite extends KittensSuite {
  import ApplySuite._
  import TestDefns._
  import TestEqInstances._

  def testApply(context: String)(implicit
      caseClassWOption: Apply[CaseClassWOption],
      optList: Apply[OptList],
      andInt: Apply[AndInt],
      interleaved: Apply[Interleaved],
      listBox: Apply[ListBox],
      record: Apply[Record]
  ): Unit = {
    implicit val isoOptList: Isomorphisms[OptList] = Isomorphisms.invariant(optList)
    implicit val isoAndInt: Isomorphisms[AndInt] = Isomorphisms.invariant(andInt)
    implicit val isoListBox: Isomorphisms[ListBox] = Isomorphisms.invariant(listBox)
    implicit val isoRecord: Isomorphisms[Record] = Isomorphisms.invariant(record)
    checkAll(s"$context.Apply[CaseClassWOption]", ApplyTests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$context.Apply[OptList]", ApplyTests[OptList].apply[Int, String, Long])
    checkAll(s"$context.Apply[AndInt]", ApplyTests[AndInt].apply[Int, String, Long])
    checkAll(s"$context.Apply[Interleaved]", ApplyTests[Interleaved].apply[Int, String, Long])
    checkAll(s"$context.Apply[ListBox]", ApplyTests[ListBox].apply[Int, String, Long])
    checkAll(s"$context.Apply[Record]", ApplyTests[Record].apply[Int, String, Long])
    checkAll(s"$context.Apply is Serializable", SerializableTests.serializable(Apply[Interleaved]))
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
    import semiInstances._
    testApply("semiauto")
  }
}

object ApplySuite {
  import TestDefns._

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]
  type Record[A] = (Mon ->> Option[A]) :: (Sun ->> List[A]) :: HNil

  object semiInstances {
    implicit val caseClassWOption: Apply[CaseClassWOption] = semiauto.apply
    implicit val optList: Apply[OptList] = semiauto.apply
    implicit val andInt: Apply[AndInt] = semiauto.apply
    implicit val interleaved: Apply[Interleaved] = semiauto.apply
    implicit val listBox: Apply[ListBox] = semiauto.apply
    implicit val record: Apply[Record] = semiauto.apply
  }
}
