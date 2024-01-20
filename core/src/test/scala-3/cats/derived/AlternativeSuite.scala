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

import cats.laws.discipline.*
import cats.{Alternative, Applicative}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class AlternativeSuite extends KittensSuite:
  import ADTs.*
  import AlternativeSuite.*

  inline def tests[F[_]]: AlternativeTests[F] =
    AlternativeTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].alternative[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].alternative[Int, String, Long])
    checkAll(s"$instance[UnCons]", tests[UnCons].alternative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Alternative[UnCons]]))

  locally:
    import auto.alternative.given
    validate("auto.alternative")

  locally:
    import semiInstances.given
    validate("semiauto.alternative")

  locally:
    import strictInstances.given
    validate("strict.semiauto.alternative")
    testNoInstance("strict.semiauto.alternative", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.alternative"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].alternative[Int, String, Long])
    checkAll(s"$instance[UnCons]", tests[UnCons].alternative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Alternative[CaseClassWOption]))

end AlternativeSuite

object AlternativeSuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]

  object semiInstances:
    given Alternative[CaseClassWOption] = semiauto.alternative
    given Alternative[OptList] = semiauto.alternative
    given Alternative[UnCons] = semiauto.alternative

  object strictInstances:
    given [F[_]: Alternative, G[_]: Applicative]: Alternative[[x] =>> F[G[x]]] = Alternative[F].compose[G]
    given Alternative[CaseClassWOption] = strict.semiauto.alternative
    given Alternative[UnCons] = strict.semiauto.alternative

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives Alternative
    case class UnCons[A](x: ADTs.UnCons[A]) derives Alternative

end AlternativeSuite
