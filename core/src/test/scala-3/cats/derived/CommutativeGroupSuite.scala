/*
 * Copyright (c) 2016 Miles Sabin
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

import cats.kernel.CommutativeGroup
import cats.kernel.laws.discipline.{CommutativeGroupTests, SerializableTests}

import scala.compiletime.*

class CommutativeGroupSuite extends KittensSuite:
  import ADTs.*
  import CommutativeGroupSuite.*

  inline def tests[A]: CommutativeGroupTests[A] =
    CommutativeGroupTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Slice]", tests[Slice].commutativeGroup)
    checkAll(s"$instance[Compared]", tests[Compared].commutativeGroup)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[CommutativeGroup[Slice]]))

  locally:
    import auto.commutativeGroup.given
    validate("auto.group")

  locally:
    import semiInstances.given
    validate("semiauto.commutativeGroup")

  locally:
    import strictInstances.given
    validate("strict.semiauto.commutativeGroup")
    testNoInstance("strict.semiauto.commutativeGroup", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.commutativeGroup"
    checkAll(s"$instance[Slice]", tests[Slice].commutativeGroup)
    checkAll(s"$instance[Compared]", tests[Compared].commutativeGroup)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(CommutativeGroup[Slice]))

end CommutativeGroupSuite

object CommutativeGroupSuite:
  import ADTs.*

  object semiInstances:
    given CommutativeGroup[Slice] = semiauto.commutativeGroup
    given CommutativeGroup[Compared] = semiauto.commutativeGroup

  object strictInstances:
    given CommutativeGroup[Slice] = strict.semiauto.commutativeGroup
    given CommutativeGroup[Compared] = strict.semiauto.commutativeGroup

  object derivedInstances:
    case class Slice(x: ADTs.Slice) derives CommutativeGroup
    case class Compared(x: ADTs.Compared) derives CommutativeGroup

end CommutativeGroupSuite
