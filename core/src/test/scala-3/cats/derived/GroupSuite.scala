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

import cats.Group
import cats.kernel.laws.discipline.{GroupTests, SerializableTests}

import scala.compiletime.*
import scala.concurrent.duration.Duration

class GroupSuite extends KittensSuite:
  import GroupSuite.*

  inline def tests[A]: GroupTests[A] =
    GroupTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Slice]", tests[Slice].group)
    checkAll(s"$instance[Compared]", tests[Compared].group)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Group[Slice]]))

  locally:
    import auto.group.given
    validate("auto.group")

  locally:
    import semiInstances.given
    validate("semiauto.group")

  locally:
    import strictInstances.given
    validate("strict.semiauto.group")
    testNoInstance("strict.semiauto.group", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.group"
    checkAll(s"$instance[Slice]", tests[Slice].group)
    checkAll(s"$instance[Compared]", tests[Compared].group)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Group[Slice]))

end GroupSuite

object GroupSuite:
  case class Slice(count: Long, percentile: Double, duration: Duration)
  case class Compared(x: Slice, y: Slice)

  object semiInstances:
    given Group[Slice] = semiauto.group
    given Group[Compared] = semiauto.group

  object strictInstances:
    given Group[Slice] = strict.semiauto.group
    given Group[Compared] = strict.semiauto.group

  object derivedInstances:
    case class Slice(x: GroupSuite.Slice) derives Group
    case class Compared(x: GroupSuite.Compared) derives Group

end GroupSuite
