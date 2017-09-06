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
import cats.Applicative
import alleycats.Pure, alleycats.std.all._
import cats.instances.int._

import TestDefns._
import MkFunctor._, MkApply._, MkApplicative._
import MkEmptyK._, MkPure._, MkFoldable._

class ApplicativeTests extends KittensSuite {


  test("Applicative[IList]") {

    val A = Applicative[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val fPlusOne = IList.fromSeq(List((_: Int) + 1))

    assert(A.ap(fPlusOne)(ns) === IList.fromSeq((2 to 11).toList))

  }

}

