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

import cats._, instances.all._, kernel.laws.discipline._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import monoid._
import TestDefns._

class MonoidSuite extends KittensSuite {
  import MonoidSuite._

  checkAll("Monoid[Foo]", MonoidTests[Foo].monoid)
  checkAll("Monoid[Rec]", MonoidTests[Rec].monoid)
}

object MonoidSuite {
  final case class Rec(i: Int, is: Option[Rec])
  object Rec {
    implicit lazy val arb: Arbitrary[Rec] = {
      Arbitrary(for {
        i <- arbitrary[Int]
        is <- arbitrary[Option[Rec]]
      } yield Rec(i, is))
    }

    implicit val eqv: Eq[Rec] =
      Eq.fromUniversalEquals
  }
}
