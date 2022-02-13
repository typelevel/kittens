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
import scala.compiletime.*

class ApplySuite extends KittensSuite {
  import ApplySuite._
  import TestDefns._

  inline def applyTests[F[_]]: ApplyTests[F] =
    ApplyTests[F](summonInline)

  inline def testApply(inline context: String): Unit = {
    given Isomorphisms[CaseClassWOption] = Isomorphisms.invariant(summonInline[Apply[CaseClassWOption]])
    given Isomorphisms[OptList] = Isomorphisms.invariant(summonInline[Apply[OptList]])
    //    given isoAndInt: Isomorphisms[AndInt] = Isomorphisms.invariant(andInt)
    given Isomorphisms[Interleaved] = Isomorphisms.invariant(summonInline[Apply[Interleaved]])
    given Isomorphisms[ListBox] = Isomorphisms.invariant(summonInline[Apply[ListBox]])

    checkAll(s"$context.Apply[CaseClassWOption]", applyTests[CaseClassWOption].apply[Int, String, Long])
    checkAll(s"$context.Apply[OptList]", applyTests[OptList].apply[Int, String, Long])
//    checkAll(s"$context.Apply[AndInt]", applyTests[AndInt].apply[Int, String, Long])
    checkAll(s"$context.Apply[Interleaved]", applyTests[Interleaved].apply[Int, String, Long])
    checkAll(s"$context.Apply[ListBox]", applyTests[ListBox].apply[Int, String, Long])
    checkAll(s"$context.Apply is Serializable", SerializableTests.serializable(summonInline[Apply[Interleaved]]))
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
