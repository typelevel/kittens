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

import cats.{ Eq, Eval, Foldable }, Eval.now

import TestDefns._
class FoldableSuite extends KittensSuite {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???

  // exists method written in terms of foldRight
  def contains[F[_]: Foldable, A: Eq](as: F[A], goal: A): Eval[Boolean] =
    as.foldRight(now(false)) { (a, lb) =>
      if (a === goal) now(true) else lb
    }

  import auto.foldable._
  import cats.instances.int._

  test("Foldable[IList]") {
    val F = Foldable[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val total = lns.sum
    assert(F.foldLeft(ns, 0)(_ + _) == total)
    assert(F.foldRight(ns, now(0))((x, ly) => ly.map(x + _)).value == total)
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
    assert(contains(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, now(List.empty[Int]))((x, lxs) => lxs.map((x + 1) :: _))
    assert(larger.value == llarge.map(_ + 1))
  }

  test("derives an instance for Interleaved[T]") {
    assertCompiles("semi.foldable[TestDefns.Interleaved]")
  }

  test("foldable.semi[Tree]") {
    val F = semi.foldable[Tree]

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
