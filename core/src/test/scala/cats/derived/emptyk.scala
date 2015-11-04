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

import alleycats.EmptyK
import shapeless._

import TestDefns._, emptyk._

class EmptyKTests extends CatsSuite {

  test("EmptyK[Option]") {
    val E = EmptyK[Option]

    assert(E.empty == None)
  }

  test("EmptyK[List]") {
    val E = EmptyK[List]

    assert(E.empty == Nil)
  }

  test("EmptyK[IList]") {
    val E = EmptyK[IList]

    assert(E.empty == INil())
  }

  test("EmptyK[λ[t => List[Option[t]]]]") {
    type LOption[t] = List[Option[t]]
    val E = EmptyK[LOption]

    assert(E.empty == List(None))
  }

  test("EmptyK[λ[t => List[List[t]]]]") {
    type LList[t] = List[List[t]]
    val E = EmptyK[LList]

    assert(E.empty == List(Nil))
  }

  test("EmptyK[λ[t => (List[t], List[t])]]") {
    type PList[t] = (List[t], List[t])
    val E = EmptyK[PList]

    assert(E.empty == (Nil, Nil))
  }

  test("EmptyK[λ[t => HNil]]") {
    type HNilK[t] = HNil
    val E = EmptyK[HNilK]

    assert(E.empty == HNil)
  }

  test("EmptyK[λ[t => IList[t] :: HNil]]") {
    type IN[t] = IList[t] :: HNil
    val E = EmptyK[IN]

    assert(E.empty == (INil() :: HNil))
  }
}
