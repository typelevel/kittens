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

import cats.Functor

import TestDefns._

import functor._, legacy._
import iterable.legacy._

class FunctorTests extends KittensSuite {

  test("Functor[IList]") {
    val F = Functor[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    assert(F.map(ns)(_+1).toList sameElements lns.map(_+1))

    // more basic checks
    val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
    val names = IList.fromSeq(lnames)
    assert(F.map(names)(_.length) sameElements lnames.map(_.length))

    // test trampolining
    val llarge = 1 to 10000
    val large = IList.fromSeq(llarge)
    assert(F.map(large)(_+1).toList sameElements llarge.map(_+1))
  }

  test("Functor[Tree]") {
    val F = Functor[Tree]

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

    assert(F.map(tree)(_.length) == expected)
  }

  test("Functor[λ[t => List[List[t]]]") {
    type LList[T] = List[List[T]]
    val F = Functor[LList]

    val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
    val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))

    assert(F.map(l)(_+1) == expected)
  }
}
