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

import cats.kernel.Semilattice
import cats.kernel.laws.discipline.{SemilatticeTests, SerializableTests}

import scala.compiletime.*

class SemilatticeSuite extends KittensSuite:
  import ADTs.*
  import SemilatticeSuite.*

  inline def tests[A]: SemilatticeTests[A] =
    SemilatticeTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Masked[String]]", tests[Masked[String]].semilattice)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Semilattice[Masked[String]]]))

  locally:
    import auto.semilattice.given
    validate("auto.semilattice")

  locally:
    import semiInstances.given
    validate("semiauto.semilattice")

  locally:
    import strictInstances.given
    validate("strict.semiauto.semilattice")
    testNoInstance("strict.semiauto.semilattice", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.semilattice"
    checkAll(s"$instance[Masked]]", tests[Masked].semilattice)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Semilattice[Masked]))

end SemilatticeSuite

object SemilatticeSuite:
  import ADTs.*

  object semiInstances:
    given Semilattice[Masked[String]] = semiauto.semilattice

  object strictInstances:
    given Semilattice[Masked[String]] = strict.semiauto.semilattice

  object derivedInstances:
    case class Masked(x: ADTs.Masked[String]) derives Semilattice

end SemilatticeSuite
