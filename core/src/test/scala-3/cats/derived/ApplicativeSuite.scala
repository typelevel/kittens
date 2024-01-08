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
import cats.{Applicative, Monoid}
import org.scalacheck.Arbitrary
import shapeless3.deriving.Const

import scala.compiletime.*

class ApplicativeSuite extends KittensSuite:
  import ADTs.*
  import ApplicativeSuite.*

  inline def tests[F[_]]: ApplicativeTests[F] =
    ApplicativeTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].applicative[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].applicative[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].applicative[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$instance[ListBox]", tests[ListBox].applicative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Applicative[Interleaved]]))

  locally:
    import auto.applicative.given
    validate("auto.applicative")

  locally:
    import semiInstances.given
    validate("semiauto.applicative")

  locally:
    import strictInstances.given
    validate("strict.semiauto.applicative")
    testNoInstance("strict.semiauto.applicative", "TopK")

  locally:
    import derivedInstances.*
    val instance = "derived.applicative"
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].applicative[Int, String, Long])
    checkAll(s"$instance[AndInt]", tests[AndInt].applicative[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].applicative[Int, String, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Applicative[Interleaved]))

end ApplicativeSuite

object ApplicativeSuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]

  object semiInstances:
    given Applicative[Box] = semiauto.applicative
    given Applicative[CaseClassWOption] = semiauto.applicative
    given Applicative[OptList] = semiauto.applicative
    given Applicative[AndInt] = semiauto.applicative
    given Applicative[Interleaved] = semiauto.applicative
    given Applicative[ListBox] = semiauto.applicative

  object strictInstances:
    given [T: Monoid]: Applicative[Const[T]] = semiauto.applicative
    given [F[_]: Applicative, G[_]: Applicative]: Applicative[[x] =>> F[G[x]]] = Applicative[F].compose[G]
    given Applicative[Box] = strict.semiauto.applicative
    given Applicative[CaseClassWOption] = strict.semiauto.applicative
    given Applicative[AndInt] = strict.semiauto.applicative
    given Applicative[Interleaved] = strict.semiauto.applicative

  object derivedInstances:
    case class CaseClassWOption[A](x: ADTs.CaseClassWOption[A]) derives Applicative
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Applicative
    case class AndInt[A](x: ApplicativeSuite.AndInt[A]) derives Applicative

end ApplicativeSuite
