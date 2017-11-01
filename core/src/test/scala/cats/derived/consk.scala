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

import alleycats.ConsK
import cats._
import shapeless._

import TestDefns._
import consk.exports._

class ConsKSuite extends KittensSuite {
  test("ConsK[IList]") {
    val C = ConsK[IList]

    assert(C.cons(23, INil()) == ICons(23, INil()))
  }

  test("ConsK[Snoc]") {
    val C = ConsK[Snoc]

    assert(C.cons(23, SNil()) == SCons(SNil(), 23))
  }
}
