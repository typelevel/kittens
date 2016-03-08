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

import alleycats.Pure, alleycats.std.all._
import cats._
import shapeless.{ Id => _, _ }

import TestDefns._
import emptyk._, pure._

class PureTests extends KittensSuite {

  test("Pure[Id]") {
    val P = Pure[Id]

    assert(P.pure(23) == 23)
  }

  test("Pure[Option]") {
    val P = Pure[Option]

    assert(P.pure(23) == Some(23))
  }

  test("Pure[Some]") {
    val P = Pure[Some]

    assert(P.pure(23) == Some(23))
  }

  test("Pure[List]") {
    val P = Pure[List]

    assert(P.pure(23) == List(23))
  }

  test("Pure[IList]") {
    val P = cachedImplicit[Pure[IList]]

    assert(P.pure(23) == ICons(23, INil()))
  }

  test("Pure[λ[t => Option[Option[t]]]]") {
    type OOption[t] = Option[Option[t]]
    val P = Pure[OOption]

    assert(P.pure(23) == Some(Some(23)))
  }

  test("Pure[λ[t => Option[List[t]]]]") {
    type OList[t] = Option[List[t]]
    val P = Pure[OList]

    assert(P.pure(23) == Some(List(23)))
  }

  test("Pure[λ[t => List[Option[t]]]]") {
    type LOption[t] = List[Option[t]]
    val P = Pure[LOption]

    assert(P.pure(23) == List(Some(23)))
  }

  test("Pure[λ[t => List[List[t]]]]") {
    type LList[t] = List[List[t]]
    val P = Pure[LList]

    assert(P.pure(23) == List(List(23)))
  }
}
