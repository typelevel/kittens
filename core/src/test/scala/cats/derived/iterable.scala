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

class IterableSuite extends KittensSuite {
  import TestDefns._
  import auto.iterable._

  test("IList[T] => Iterable[T]") {
    // test trampolining
    val llarge = 1 to 10000
    val llargeTotal = llarge.sum

    val large = IList.fromSeq(llarge)

    val I: Iterable[Int] = large

    val i = I.iterator
    val total = i.sum
    assert(total == llargeTotal)

    val i2 = I.iterator
    assert(i2 sameElements llarge.iterator)
  }

  test("(T, T, T) => Iterable[T]") {
    type F[T] = (T, T, T)

    val I: Iterable[Int] = MkIterable[F].iterable((1, 2, 3))
    val i = I.iterator
    val total = i.sum
    assert(total == 6)
  }

  test("List[(T, Option[T])] => Iterable[T]") {
    type F[T] = List[(T, Option[T])]

    val I: Iterable[Int] = MkIterable[F].iterable(List((1, Some(2)), (3, None)))
    val i = I.iterator
    val total = i.sum
    assert(total == 6)
  }

  test("(List[T], List[T], List[T]) => Iterable[T]") {
    type F[T] = (List[T], List[T], List[T])

    val I: Iterable[Int] = MkIterable[F].iterable((List(1, 2, 3), List(4), List(5, 6)))
    val i = I.iterator
    val total = i.sum
    assert(total == 21)
  }

  test("List[List[T]] => Iterable[T]") {
    type F[T] = List[List[T]]

    val I: Iterable[Int] = MkIterable[F].iterable(List(List(1, 2, 3), List(4), List(5, 6)))
    val i = I.iterator
    val total = i.sum
    assert(total == 21)
  }

  test("Tree[T] => Iterable[T]") {
    val tree: Tree[String] =
      Node(
        Leaf("quux"),
        Node(
          Leaf("foo"),
          Leaf("wibble")
        )
      )

    val I: Iterable[String] = MkIterable[Tree].iterable(tree)
    assert(I.map(_.length).sum == 13)
  }

  test("Interleaved[T] => Iterable[T]") {
    val interleaved = Interleaved(42, 313, 3, List(1, 2, 3, 5, 7), "kittens")
    val i = semiauto.iterable[TestDefns.Interleaved, Int](interleaved)
    assert(i.toList == List(313, 1, 2, 3, 5, 7))
  }
}
