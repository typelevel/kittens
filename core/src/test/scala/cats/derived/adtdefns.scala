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

import cats.Eq
import cats.instances.all._
import org.scalacheck.{Cogen, Arbitrary}

import scala.annotation.tailrec

object TestDefns {

  final case class Box[+A](content: A)
  object Box {

    implicit def eqv[A: Eq]: Eq[Box[A]] =
      Eq.by(_.content)

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Box[A]] =
      Arbitrary(Arbitrary.arbitrary[A].map(apply))
  }

  final case class Recursive(i: Int, is: Option[Recursive])
  object Recursive {

    implicit val eqv: Eq[Recursive] =
      Eq.fromUniversalEquals

    implicit lazy val arbitrary: Arbitrary[Recursive] =
      Arbitrary(for {
        i <- Arbitrary.arbitrary[Int]
        is <- Arbitrary.arbitrary[Option[Recursive]]
      } yield Recursive(i, is))
  }

  final case class Interleaved[T](i: Int, t: T, d: Double, tt: List[T], s: String)
  object Interleaved {

    implicit def eqv[T: Eq]: Eq[Interleaved[T]] =
      Eq.by(i => (i.i, i.t, i.d, i.tt, i.s))

    implicit def arbitrary[T: Arbitrary]: Arbitrary[Interleaved[T]] =
      Arbitrary(for {
        i <- Arbitrary.arbitrary[Int]
        t <- Arbitrary.arbitrary[T]
        d <- Arbitrary.arbitrary[Double]
        tt <- Arbitrary.arbitrary[List[T]]
        s <- Arbitrary.arbitrary[String]
      } yield Interleaved(i, t, d, tt, s))
  }

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object IList {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[IList[A]] =
      Arbitrary(Arbitrary.arbitrary[Seq[A]].map(fromSeq))

    implicit def cogen[A: Cogen]: Cogen[IList[A]] =
      Cogen[Seq[A]].contramap(toList)

    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight[IList[T]](INil())(ICons.apply)

    def toList[T](l: IList[T]): List[T] = {
      @tailrec def loop(il: IList[T], acc: List[T]): List[T] = il match {
        case INil() => acc.reverse
        case ICons(h, t) => loop(t, h :: acc)
      }

      loop(l, Nil)
    }
  }

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {

    def fromSeq[T](ts: Seq[T]): Snoc[T] =
      ts.foldLeft[Snoc[T]](SNil())(SCons.apply)
  }

  sealed trait Tree[T]
  final case class Leaf[T](t: T) extends Tree[T]
  final case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  final case class Foo(i: Int, b: Option[String])
  object Foo {

    implicit val eqv: Eq[Foo] =
      Eq.fromUniversalEquals

    implicit val cogen: Cogen[Foo] =
      Cogen[Int].contramap(_.i)

    implicit val arbitrary: Arbitrary[Foo] =
      Arbitrary(for {
        i <- Arbitrary.arbitrary[Int]
        b <- Arbitrary.arbitrary[Option[String]]
      } yield Foo(i, b))
  }

  case class Inner(i: Int)
  case class Outer(in: Inner)

  object Inner {

    implicit val arbitrary: Arbitrary[Inner] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val cogen: Cogen[Inner] =
      Cogen[Int].contramap(_.i)
  }

  object Outer {

    implicit val arbitrary: Arbitrary[Outer] =
      Arbitrary(Arbitrary.arbitrary[Inner].map(apply))

    implicit val cogen: Cogen[Outer] =
      Cogen[Inner].contramap(_.in)
  }

  sealed trait IntTree
  final case class IntLeaf(t: Int) extends IntTree
  final case class IntNode(l: IntTree, r: IntTree) extends IntTree

  sealed trait GenericAdt[T]
  final case class GenericAdtCase[T](v: Option[T]) extends GenericAdt[T]
  final case class CaseClassWOption[T](a: Option[T])

  final case class First(value: String)
  final case class Second(value: String)
  final case class Middle(first: First, second: Option[Second])
  final case class Top(middle: Middle)

  final case class Address(street: String, city: String, state: String)
  final case class ContactInfo(phoneNumber: String, address: Address)
  final case class People(name: String, contactInfo: ContactInfo)

  final case class ListField(a: String, b: List[ListFieldChild])
  final case class ListFieldChild(c: Int)

  final case class Large(
    bar1: String,
    bar2: Int,
    bar3: Boolean,
    bar4: Large2,
    bar5: List[String],
    bar6: Set[Boolean],
    bar7: Double,
    bar8: Long,
    bar9: Char,
    bar10: Float,
    bar11: String,
    bar12: Map[String, Int],
    bar13: Boolean,
    bar14: Option[String],
    bar15: List[String],
    bar16: Set[Boolean],
    bar17: Double,
    bar18: Long,
    bar19: Char,
    bar20: Float
  )

  final case class Large2(
    bar1: String,
    bar2: Int,
    bar3: Boolean,
    bar4: Option[String],
    bar5: List[String],
    bar6: Set[Boolean],
    bar7: Double,
    bar8: Long,
    bar9: Char,
    bar10: Float,
    bar11: String,
    bar12: Map[String, Int],
    bar13: Boolean,
    bar14: Option[String],
    bar15: List[String],
    bar16: Set[Boolean],
    bar17: Double,
    bar18: Long,
    bar19: Char,
    bar20: Float,
    bar21: String
  )

  final case class Large3(
    bar1: String,
    bar2: Int,
    bar3: Boolean,
    bar4: Option[String],
    bar5: List[String],
    bar6: Set[Boolean],
    bar7: Double,
    bar8: Long,
    bar9: Char,
    bar10: Float,
    bar11: String,
    bar12: Map[String, Int],
    bar13: Boolean,
    bar14: Option[String],
    bar15: List[String],
    bar16: Set[Boolean],
    bar17: Double,
    bar18: Long,
    bar19: Char,
    bar20: Float,
    bar21: String
  )

  final case class Large4(
    bar1: String,
    bar2: Int,
    bar3: Boolean,
    bar4: Large5,
    bar5: List[String],
    bar6: List[Boolean],
    bar7: Double,
    bar8: Long,
    bar9: Char,
    bar10: Float,
    bar11: String,
    bar12: String,
    bar13: Boolean,
    bar14: Option[String],
    bar15: List[String],
    bar16: List[Boolean],
    bar17: Double,
    bar18: Long,
    bar19: Char,
    bar20: Float
  )

  final case class Large5(
    bar1: String,
    bar2: Int,
    bar3: Boolean,
    bar4: Option[String],
    bar5: List[String],
    bar6: List[Boolean],
    bar7: Double,
    bar8: Long,
    bar9: Char,
    bar10: Float,
    bar11: String,
    bar12: Int,
    bar13: Boolean,
    bar14: Option[String],
    bar15: List[String],
    bar16: List[Boolean],
    bar17: Double,
    bar18: Long,
    bar19: Char,
    bar20: Float,
    bar21: String
  )
}
