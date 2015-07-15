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

import cats.Pointed
import shapeless._

import TestDefns._

class PointedTests extends CatsSuite {
  import cats.derived.pointed._

  test("Pointed[Id]") {
    val P = Pointed[Id]

    assert(P.point(23) == 23)
  }

  test("Pointed[Option]") {
    val P = Pointed[Option]

    assert(P.point(23) == Some(23))
  }

  test("Pointed[List]") {
    val P = Pointed[List]

    assert(P.point(23) == List(23))
  }

  test("Pointed[λ[t => t :: HNil]]") {
    type TNil[t] = t :: HNil
    val P = Pointed[TNil]

    assert(P.point(23) == (23 :: HNil))
  }

  test("Pointed[IList]") {
    val P = Pointed[IList]

    assert(P.point(23) == ICons(23, INil()))
  }

  test("Pointed[λ[t => List[Option[t]]]]") {
    type LOption[t] = List[Option[t]]
    val P = Pointed[LOption]

    assert(P.point(23) == List(Some(23)))
  }

  test("Pointed[λ[t => List[List[t]]]]") {
    type LList[t] = List[List[t]]
    val P = Pointed[LList]

    assert(P.point(23) == List(List(23)))
  }
}

