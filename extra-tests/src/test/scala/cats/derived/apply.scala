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
import cats.{ Eq, Eval, Foldable, Functor, Apply}, Eval.now

import alleycats.Pure, alleycats.std.all._

import cats.std.int._

import TestDefns._
import shapeless.Cached

//import apply._ todo: export hook conflicts with functor

class ApplyTests extends KittensSuite {
  import emptyk._, pure._

  import ApplyTests._

  test("Apply[CaseClassWOption]") {
    import Implicits.{ccO, fccO}

    val M = MkApply[CaseClassWOption]
    val p1 = CaseClassWOption(Option(1))
    val f =  CaseClassWOption(Option((_: Int) + 1))

    assert(M.ap(f)(p1) == CaseClassWOption(Option(2)))
    assert(M.ap(f)(CaseClassWOption(None)) == CaseClassWOption(None))
    val emptyF = CaseClassWOption[Int => Int](None)
    assert(M.ap(emptyF)(CaseClassWOption(Option(1))) == CaseClassWOption(None))

  }

  test("Apply[IList]") {
    import Implicits.{l, fl}

    val A = MkApply[IList]

    // some basic sanity checks
    val lns = (1 to 10).toList
    val ns = IList.fromSeq(lns)
    val fPlusOne = IList.fromSeq(List((_: Int) + 1))

    assert(A.ap(fPlusOne)(ns) === IList.fromSeq((2 to 11).toList))


    // more basic checks
    val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
    val names = IList.fromSeq(lnames)
    val fLength = IList.fromSeq(List((_: String).length))
    val lengths = A.ap(fLength)(names)

    assert(lengths === IList.fromSeq(lnames.map(_.length)))

    // test trampolining
    val llarge = 1 to 10000
    val large = IList.fromSeq(llarge)
    val resultList = MkIterable[IList].iterable(A.ap(fPlusOne)(large)).toList

    assert(resultList === llarge.map(_ + 1))
  }

  test("Apply[λ[t => List[List[t]]]") {
    import Implicits.{ll, fll}

    val A = MkApply[LList]

    val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
    val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))
    val fPlusOne: LList[Int => Int] = List(List((_: Int) + 1))

    assert(A.ap(fPlusOne)(l) == expected)
  }
}

class ApplyWithoutEmptyKTests extends KittensSuite {

  import ApplyTests.Implicits.{t, ft}

  test("Apply[Tree]") {
    val A = MkApply[Tree]

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
    val fLength: Tree[String => Int] = Leaf((_: String).length)

    assert(A.ap(fLength)(tree) == expected)
  }

}


object ApplyTests {
  import emptyk._, pure._
  type LList[T] = List[List[T]]
  import shapeless.cachedImplicit

  //Has to isolate Foldable and Functor derivation,
  // on scala 2.10.x they can't co exist in the same scope
  // todo: remove this manual step once Foldable and Functor can co exist on 2.10.x
  object FoldableDeriveProxy {
    import foldable._ , foldable.legacy._

    lazy val ccO = cachedImplicit[Foldable[CaseClassWOption]]
    lazy val iList = cachedImplicit[Foldable[IList]]
    lazy val t = cachedImplicit[Foldable[Tree]]
    lazy val lList = cachedImplicit[Foldable[LList]]
  }

  object FunctorDeriveProxy {
    import functor._ , functor.legacy._
    lazy val ccO = cachedImplicit[Functor[CaseClassWOption]]
    lazy val iList = cachedImplicit[Functor[IList]]
    lazy val t = cachedImplicit[Functor[Tree]]
    lazy val lList = cachedImplicit[Functor[LList]]
  }

  object Implicits {
    implicit lazy val ccO: Foldable[CaseClassWOption] = FoldableDeriveProxy.ccO
    implicit lazy val l:  Foldable[IList] = FoldableDeriveProxy.iList
    implicit lazy val t: Foldable[Tree] = FoldableDeriveProxy.t
    implicit lazy val ll: Foldable[LList] = FoldableDeriveProxy.lList
    implicit lazy val fccO: Functor[CaseClassWOption] = FunctorDeriveProxy.ccO
    implicit lazy val fl: Functor[IList] = FunctorDeriveProxy.iList
    implicit lazy val ft: Functor[Tree] = FunctorDeriveProxy.t
    implicit lazy val fll: Functor[LList] = FunctorDeriveProxy.lList
  }

}
