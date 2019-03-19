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
import org.scalacheck.{Cogen, Arbitrary}, Arbitrary.arbitrary
import scala.annotation.tailrec

object TestDefns {

  case class Interleaved[T](i: Int, t: T, d: Double, tt: List[T], s: String)

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object IList {
    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight(INil[T](): IList[T])(ICons(_, _))

    def toList[T](l: IList[T]): List[T] = {
      @tailrec def loop(il: IList[T], acc: List[T]): List[T] = il match {
        case INil() => acc.reverse
        case ICons(h, t) => loop(t, h :: acc)
      }

      loop(l, Nil)
    }

  }

  implicit def arbIList[A:Arbitrary]: Arbitrary[IList[A]] = Arbitrary(
    arbitrary[Seq[A]].map(IList.fromSeq))

  implicit def cogenIList[A:Cogen]: Cogen[IList[A]] =
    Cogen[Seq[A]].contramap(IList.toList)

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {
    def fromSeq[T](ts: Seq[T]): Snoc[T] =
      ts.foldLeft(SNil[T](): Snoc[T])(SCons(_, _))
  }

  sealed trait Tree[T]
  final case class Leaf[T](t: T) extends Tree[T]
  final case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  case class CaseClassWOption[T](a: Option[T])

  final case class Foo(i: Int, b: Option[String])

  case class Inner(i: Int)
  case class Outer(in: Inner)

  sealed trait IntTree
  final case class IntLeaf(t: Int) extends IntTree
  final case class IntNode(l: IntTree, r: IntTree) extends IntTree

  sealed trait GenericAdt[T]
  case class GenericAdtCase[T](v: Option[T]) extends GenericAdt[T]

  implicit val arbFoo: Arbitrary[Foo] =
    Arbitrary(for {
      i <- arbitrary[Int]
      b <- arbitrary[Option[String]]
    } yield Foo(i, b))


  implicit val cogenFoo: Cogen[Foo] = Cogen.cogenInt.contramap[Foo](_.i)

  implicit val arbInner: Arbitrary[Inner] =
    Arbitrary(for {
      i <- arbitrary[Int]
    } yield Inner(i))

 implicit val cogenInner: Cogen[Inner] =
   Cogen[Int].contramap(_.i)

  implicit val cogenOuter: Cogen[Outer] =
   Cogen[Inner].contramap(_.in)

 implicit val arbOuter: Arbitrary[Outer] =
    Arbitrary(for {
      i <- arbitrary[Inner]
    } yield Outer(i))

  implicit val eqFoo: Eq[Foo] =
    Eq.fromUniversalEquals

  final case class First(value: String)
  final case class Second(value: String)
  final case class Middle(first: First, second: Option[Second])
  final case class Top(middle: Middle)

  case class Address(street: String, city: String, state: String)
  case class ContactInfo(phoneNumber: String, address: Address)
  case class People(name: String, contactInfo: ContactInfo)

  case class Large(
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

  case class Large2(
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


  case class Large3(
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
  case class Large4(
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

  case class Large5(
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

  case class ListField(a: String, b: List[ListFieldChild])

  case class ListFieldChild(c: Int)
}
