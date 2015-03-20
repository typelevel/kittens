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

import cats.{ Eq, Fold, Foldable }, Fold.{ Continue, Return, Pass }

import TestDefns._

class FoldableTests extends CatsSuite {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???

  // exists method written in terms of foldRight
  def exists[F[_]: Foldable, A: Eq](as: F[A], goal: A): cats.Lazy[Boolean] =
    Foldable[F].foldRight(as, cats.Lazy(false)) { a =>
      if (a === goal) Return(true) else Pass
    }

  import cats.derived.foldable._

  test("Foldable[IList]") {
    val F = Foldable[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val total = lns.sum
    assert(F.foldLeft(ns, 0)(_ + _) == total)
    assert(F.foldRight(ns, cats.Lazy(0))(x => Continue(x + _)).value == total)
    assert(F.fold(ns) == total)

    // more basic checks
    val lnames = List("Aaron", "Betty", "Calvin", "Deirdra")
    val names = IList.fromSeq(lnames)
    assert(F.foldMap(names)(_.length) == lnames.map(_.length).sum)

    // test trampolining
    val llarge = 1 to 10000
    val large = IList.fromSeq(llarge)
    val largeTotal = llarge.sum
    assert(F.foldLeft(large, 0)(_ + _) == largeTotal)
    assert(F.fold(large) == largeTotal)
    assert(exists(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, cats.Lazy(List.empty[Int]))(x => Continue((x + 1) :: _))
    assert(larger.value == llarge.map(_ + 1))
  }

  test("Foldable[Tree]") {
    val F = Foldable[Tree]

    val tree: Tree[String] =
      Node(
        Leaf("quux"),
        Node(
          Leaf("foo"),
          Leaf("wibble")
        )
      )

    assert(F.foldLeft(tree, 0)(_ + _.length) == 13)
  }
}
