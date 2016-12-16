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

import cats.{Eval, Foldable}, Eval.now

import alleycats.{EmptyK, ConsK, Pure}, alleycats.std.all._

import cats.instances.int._

import TestDefns._


//import monad._ todo: importing causes compiler to hang
import shapeless.the



class MonadTests extends KittensSuite {

  test("Monad[CaseClassWOption]") {
    import emptyk._, pure._, foldable._, foldable.legacy._

    val M = MkMonad[CaseClassWOption]
    val p1 = M.pure(1)
    val f = (i : Int) => M.pure(i + 1)
    assert(M.flatMap(p1)(f) == CaseClassWOption(Option(2)))

    assert(M.flatMap(CaseClassWOption(None))(f) == CaseClassWOption(None))

  }

  test("Monad[IList]") {
    import emptyk._, pure._, consk.exports._, foldable._, foldable.legacy._

    val A = MkMonad[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val fPlusOne = (i: Int) => A.pure(i + 1)

    assert(A.flatMap(ns)(fPlusOne) === IList.fromSeq((2 to 11).toList))


    // more basic checks
    val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
    val names = IList.fromSeq(lnames)
    val fLength = (s:String) => A.pure(s.length)
    val lengths = A.flatMap(names)(fLength)

    assert(lengths === IList.fromSeq(lnames.map(_.length)))

    //trampoline
    val llarge = 1 to 10000
    val large = IList.fromSeq(llarge)
    val result = A.flatMap(large)(fPlusOne)
    val resultList = MkIterable[IList].iterable(result).toList
    assert(resultList === llarge.map(_+1))
  }

}
