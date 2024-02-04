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

import cats.kernel.BoundedSemilattice
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, SerializableTests}

import scala.compiletime.*

class BoundedSemilatticeSuite extends KittensSuite:
  import ADTs.*
  import BoundedSemilatticeSuite.*

  inline def tests[A]: BoundedSemilatticeTests[A] =
    BoundedSemilatticeTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Masked[String]]", tests[Masked[String]].boundedSemilattice)
    checkAll(
      s"$instance is Serializable",
      SerializableTests.serializable(summonInline[BoundedSemilattice[Masked[String]]])
    )

  locally:
    import auto.boundedSemilattice.given
    validate("auto.boundedSemilattice")

  locally:
    import semiInstances.given
    validate("semiauto.boundedSemilattice")

  locally:
    import strictInstances.given
    validate("strict.semiauto.boundedSemilattice")
    testNoInstance("strict.semiauto.boundedSemilattice", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.boundedSemilattice"
    checkAll(s"$instance[Masked]]", tests[Masked].boundedSemilattice)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(BoundedSemilattice[Masked]))

end BoundedSemilatticeSuite

object BoundedSemilatticeSuite:
  import ADTs.*

  object semiInstances:
    given BoundedSemilattice[Masked[String]] = semiauto.boundedSemilattice

  object strictInstances:
    given BoundedSemilattice[Masked[String]] = strict.semiauto.boundedSemilattice

  object derivedInstances:
    case class Masked(x: ADTs.Masked[String]) derives BoundedSemilattice

end BoundedSemilatticeSuite
