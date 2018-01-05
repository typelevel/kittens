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

package cats
package derived

import cats.instances.all._
import cats.syntax.FunctorSyntax
import shapeless.test.illTyped
import org.scalatest.FreeSpec
import TestDefns._


class FunctorSuite extends FreeSpec with FunctorSyntax {

  "semi auto derivation" - {
    "for a generic ADT respects existing instances" in {
      implicit val F = derive.functor[GenericAdt]
      val adt: GenericAdt[Int] = GenericAdtCase(Some(2))
      assert(adt.map(_ + 1) == GenericAdtCase(Some(3)))
    }

    "for an IList" in {
      implicit val F = derive.functor[IList]

      // some basic sanity checks
      val lns = (1 to 10).toList
      val ns = IList.fromSeq(lns)
      assert(IList.toList(ns.map(_ + 1)) == lns.map(_ + 1))

      // more basic checks
      val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
      val names = IList.fromSeq(lnames)
      assert(IList.toList(names.map(_.length)) == lnames.map(_.length))

      // test trampolining
      val llarge = 1 to 10000
      val large = IList.fromSeq(llarge)
      assert(IList.toList(large.map(_ + 1)) == llarge.map(_ + 1))
    }

    "for a Tree" in {
      implicit val F = derive.functor[Tree]

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

      assert(tree.map(_.length) == expected)
    }

    "for a nested List[List[_]] (with alias)" in {
      illTyped("derive.functor[λ[t => List[List[t]]]]")
      type LList[T] = List[List[T]]
      val F = derive.functor[LList]

      val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
      val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))

      assert(F.map(l)(_ + 1) == expected)
    }

    "for a pair on the left (with alias)" in {
      illTyped("derive.functor[(?, String)]")
      def F[R]: Functor[(?, R)] = {
        type Pair[L] = (L, R)
        derive.functor[Pair]
      }

      val pair = (42, "shapeless")
      assert(F[String].map(pair)(_ / 2) == (21, "shapeless"))
    }

    "for a pair on the right" in {
      def F[L]: Functor[(L, ?)] = derive.functor[(L, ?)]
      val pair = (42, "shapeless")
      assert(F[Int].map(pair)(_.length) == (42, 9))
    }
  }

  "full auto derivation" - {
    import derived.functor._

    "for a generic ADT respects existing instances" in {
      val adt: GenericAdt[Int] = GenericAdtCase(Some(2))
      assert(adt.map(_ + 1) == GenericAdtCase(Some(3)))
    }

    "for an IList" in {
      // some basic sanity checks
      val lns = (1 to 10).toList
      val ns = IList.fromSeq(lns)
      assert(IList.toList(ns.map(_ + 1)) == lns.map(_ + 1))

      // more basic checks
      val lnames = List("Aaron", "Betty", "Calvin", "Deirdre")
      val names = IList.fromSeq(lnames)
      assert(IList.toList(names.map(_.length)) == lnames.map(_.length))

      // test trampolining
      val llarge = 1 to 10000
      val large = IList.fromSeq(llarge)
      assert(IList.toList(large.map(_ + 1)) == llarge.map(_ + 1))
    }

    "for a Tree" in {
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

      assert(tree.map(_.length) == expected)
    }

    "for a nested List[List[_]] (with alias)" in {
      illTyped("Functor[λ[t => List[List[t]]]]")
      type LList[T] = List[List[T]]
      val l = List(List(1), List(2, 3), List(4, 5, 6), List(), List(7))
      val expected = List(List(2), List(3, 4), List(5, 6, 7), List(), List(8))
      assert(Functor[LList].map(l)(_ + 1) == expected)
    }

    "for a pair on the left (with alias)" in {
      illTyped("derive.functor[(?, String)]")
      def F[R]: Functor[(?, R)] = {
        type Pair[L] = (L, R)
        Functor[Pair]
      }

      val pair = (42, "shapeless")
      assert(F[String].map(pair)(_ / 2) == (21, "shapeless"))
    }

    "for a pair on the right" in {
      def F[L]: Functor[(L, ?)] = Functor[(L, ?)]
      val pair = (42, "shapeless")
      assert(F[Int].map(pair)(_.length) == (42, 9))
    }
  }
}
