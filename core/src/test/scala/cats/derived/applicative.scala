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
import cats.{ Eq, Eval, Foldable, Functor, Applicative}, Eval.now

import alleycats.Pure, alleycats.std.all._

import cats.std.int._

import TestDefns._

import emptyk._, pure._, functor._, functor.legacy._, foldable._, foldable.legacy._

//import applicative._



class ApplicativeTests extends KittensSuite {


  test("Applicative[IList]") {

    val A = MkApplicative[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val fPlusOne = A.pure((_: Int) + 1)

    assert(A.ap(fPlusOne)(ns) === IList.fromSeq((2 to 11).toList))


    // more basic checks
    val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
    val names = IList.fromSeq(lnames)
    val fLength = A.pure((_:String).length)
    val lengths = A.ap(fLength)(names)

    assert(lengths === IList.fromSeq(lnames.map(_.length)))

    // test trampolining todo: not working yet
    //    val llarge = 1 to 10000
    //    val large = IList.fromSeq(llarge)
    //
    //    assert(A.ap(fPlusOne)(large) === IList.fromSeq(llarge.map(_+1)))
  }

  test("Applicative[Tree]") {
    val A = MkApplicative[Tree]

    val tree: Tree[String] =
      Node(
        Leaf("quux"),
        Node(
          Leaf("foo"),
          Leaf("wibble")
        )
      )

    val expected: Tree[Int] =
      Node(
        Leaf(4),
        Node(
          Leaf(3),
          Leaf(6)
        )
      )
    val fLength = A.pure((_:String).length)

    assert(A.ap(fLength)(tree) == expected)
  }

  test("Applicative[λ[t => List[List[t]]]") {
    type LList[T] = List[List[T]]
    val A = MkApplicative[LList]

    val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
    val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))
    val fPlusOne = A.pure((_: Int) + 1)

    assert(A.ap(fPlusOne)(l) == expected)
  }
}
