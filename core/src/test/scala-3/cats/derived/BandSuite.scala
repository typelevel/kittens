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

import cats.kernel.Band
import cats.kernel.laws.discipline.{BandTests, SerializableTests}

import scala.compiletime.*

class BandSuite extends KittensSuite:
  import ADTs.*
  import BandSuite.*

  inline def tests[A]: BandTests[A] =
    BandTests[A](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Masked[String]]", tests[Masked[String]].band)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Band[Masked[String]]]))

  locally:
    import auto.band.given
    validate("auto.band")

  locally:
    import semiInstances.given
    validate("semiauto.band")

  locally:
    import strictInstances.given
    validate("strict.semiauto.band")
    testNoInstance("strict.semiauto.band", "Top")

  locally:
    import derivedInstances.*
    val instance = "derived.band"
    checkAll(s"$instance[Masked]]", tests[Masked].band)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Band[Masked]))

end BandSuite

object BandSuite:
  import ADTs.*

  object semiInstances:
    given Band[Masked[String]] = semiauto.band

  object strictInstances:
    given Band[Masked[String]] = strict.semiauto.band

  object derivedInstances:
    case class Masked(x: ADTs.Masked[String]) derives Band

end BandSuite
