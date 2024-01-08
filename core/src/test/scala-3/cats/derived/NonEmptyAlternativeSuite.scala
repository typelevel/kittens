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
import cats.{Applicative, NonEmptyAlternative}
import org.scalacheck.Arbitrary

import scala.compiletime.*

class NonEmptyAlternativeSuite extends KittensSuite:
  import ADTs.*
  import NonEmptyAlternativeSuite.*

  inline def tests[F[_]]: NonEmptyAlternativeTests[F] =
    NonEmptyAlternativeTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].nonEmptyAlternative[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].nonEmptyAlternative[Int, String, Long])
    checkAll(s"$instance[UnCons]", tests[UnCons].nonEmptyAlternative[Int, String, Long])
    checkAll(
      s"$instance is Serializable",
      SerializableTests.serializable(summonInline[NonEmptyAlternative[CaseClassWOption]])
    )

  locally:
    import auto.nonEmptyAlternative.given
    validate("auto.nonEmptyAlternative")

  locally:
    import semiInstances.given
    validate("semiauto.nonEmptyAlternative")

  locally:
    import strictInstances.given
    validate("strict.semiauto.nonEmptyAlternative")
    testNoInstance("strict.semiauto.nonEmptyAlternative", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.nonEmptyAlternative"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].nonEmptyAlternative[Int, String, Long])
    checkAll(s"$instance[UnCons]", tests[UnCons].nonEmptyAlternative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Applicative[CaseClassWOption]))

end NonEmptyAlternativeSuite

object NonEmptyAlternativeSuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]
  type UnCons[+A] = (Option[A], Vector[A])

  object semiInstances:
    given NonEmptyAlternative[CaseClassWOption] = semiauto.nonEmptyAlternative
    given NonEmptyAlternative[OptList] = semiauto.nonEmptyAlternative
    given NonEmptyAlternative[UnCons] = semiauto.nonEmptyAlternative

  object strictInstances:
    given [F[_]: NonEmptyAlternative, G[_]: Applicative]: NonEmptyAlternative[[x] =>> F[G[x]]] =
      NonEmptyAlternative[F].compose[G]
    given NonEmptyAlternative[CaseClassWOption] = strict.semiauto.nonEmptyAlternative
    given NonEmptyAlternative[UnCons] = strict.semiauto.nonEmptyAlternative

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives NonEmptyAlternative
    case class UnCons[A](x: NonEmptyAlternativeSuite.UnCons[A]) derives NonEmptyAlternative

end NonEmptyAlternativeSuite
