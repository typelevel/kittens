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

import cats.Eq
import cats.platform.Platform
import cats.syntax.AllSyntax
import munit.DisciplineSuite
import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Cogen, Gen}
import shapeless._
import shapeless.labelled._

/** An opinionated stack of traits to improve consistency and reduce boilerplate in Kittens tests. Note that unlike the
  * corresponding CatsSuite in the Cat project, this trait does not mix in any instances.
  */
abstract class KittensSuite extends DisciplineSuite with AllSyntax {
  override val scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters
    .withMinSuccessfulTests(if (Platform.isJvm) 50 else 5)
    .withMaxDiscardRatio(if (Platform.isJvm) 5 else 50)
    .withWorkers(if (Platform.isJvm) 2 else 1)
    .withMaxSize(if (Platform.isJvm) 10 else 5)
    .withMinSize(0)

  implicit def fieldTypeArb[K, V: Arbitrary]: Arbitrary[FieldType[K, V]] =
    Arbitrary(Arbitrary.arbitrary[V].map(field[K].apply))

  implicit def fieldTypeCogen[K, V: Cogen]: Cogen[FieldType[K, V]] =
    Cogen[V].contramap(identity)

  implicit def fieldTypeEq[K, V: Eq]: Eq[FieldType[K, V]] =
    Eq.by(identity[V])

  implicit val hNilArb: Arbitrary[HNil] = Arbitrary(Gen.const(HNil))
  implicit val hNilCogen: Cogen[HNil] = Cogen[Unit].contramap(_ => ())
  implicit val hNilEq: Eq[HNil] = Eq.allEqual

  implicit val cNilArb: Arbitrary[CNil] = Arbitrary(Gen.fail)
  implicit val cNilCogen: Cogen[CNil] = Cogen[Unit].contramap(_ => ())
  implicit val cNilEq: Eq[CNil] = Eq.allEqual

  implicit def hConsArb[H: Arbitrary, T <: HList: Arbitrary]: Arbitrary[H :: T] =
    Arbitrary(Arbitrary.arbitrary[(H, T)].map { case (h, t) => h :: t })

  implicit def hConsCogen[H: Cogen, T <: HList: Cogen]: Cogen[H :: T] =
    Cogen[(H, T)].contramap { case h :: t => (h, t) }

  implicit def hConsEq[H: Eq, T <: HList: Eq]: Eq[H :: T] =
    Eq.by { case h :: t => (h, t) }

  implicit def cConsArb[L: Arbitrary, R <: Coproduct: Arbitrary]: Arbitrary[L :+: R] =
    Arbitrary(Arbitrary.arbitrary[Either[L, R]].map {
      case Left(l) => Inl(l)
      case Right(r) => Inr(r)
    })

  implicit def cConsCogen[L: Cogen, R <: Coproduct: Cogen]: Cogen[L :+: R] =
    Cogen[Either[L, R]].contramap {
      case Inl(l) => Left(l)
      case Inr(r) => Right(r)
    }

  implicit def cConsEq[L: Eq, R <: Coproduct: Eq]: Eq[L :+: R] =
    Eq.by {
      case Inl(l) => Left(l)
      case Inr(r) => Right(r)
    }
}
