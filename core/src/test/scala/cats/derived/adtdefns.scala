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

import cats.{Eq, Eval}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.annotation.tailrec

object TestDefns {

  sealed trait Rgb
  case object Red extends Rgb
  case object Green extends Rgb
  case object Blue extends Rgb

  final case class ComplexProduct[T](lbl: String, set: Set[T], fns: Vector[() => T], opt: Eval[Option[T]])
  object ComplexProduct {

    implicit def arbitrary[T: Arbitrary]: Arbitrary[ComplexProduct[T]] =
      Arbitrary(for {
        lbl <- Arbitrary.arbitrary[String]
        set <- Arbitrary.arbitrary[Set[T]]
        vec <- Arbitrary.arbitrary[Vector[T]]
        fns = vec.map(x => () => x)
        opt <- Arbitrary.arbitrary[Option[T]]
      } yield ComplexProduct(lbl, set, fns, Eval.now(opt)))
  }

  final case class Box[+A](content: A)
  object Box {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Box[A]] =
      Arbitrary(Arbitrary.arbitrary[A].map(apply))

    implicit def cogen[A: Cogen]: Cogen[Box[A]] =
      Cogen[A].contramap(_.content)
  }

  final case class Recursive(i: Int, is: Option[Recursive])
  object Recursive extends ((Int, Option[Recursive]) => Recursive) {

    implicit val arbitrary: Arbitrary[Recursive] = {
      def recursive(size: Int): Gen[Recursive] = for {
        i <- Arbitrary.arbitrary[Int]
        is <- if (size <= 0) Gen.const(None) else Gen.option(recursive(size / 2))
      } yield Recursive(i, is)
      Arbitrary(Gen.sized(recursive))
    }

    implicit val cogen: Cogen[Recursive] = {
      @tailrec def perturb(seed: Seed, rec: Recursive): Seed = {
        val i = Cogen[Int].perturb(seed, rec.i)
        rec.is match {
          case Some(is) => perturb(i, is)
          case None => i
        }
      }

      Cogen(perturb _)
    }
  }

  final case class Interleaved[T](i: Int, t: T, l: Long, tt: List[T], s: String)
  object Interleaved {

    implicit def arbitrary[T: Arbitrary]: Arbitrary[Interleaved[T]] =
      Arbitrary(Arbitrary.arbitrary[(Int, T, Long, List[T], String)].map((apply[T] _).tupled))

    implicit def cogen[T: Cogen]: Cogen[Interleaved[T]] =
      Cogen[(Int, T, Long, List[T], String)].contramap(unapply(_).get)
  }

  case class Bivariant[A](run: A => Boolean, store: A)
  object Bivariant {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Bivariant[A]] = Arbitrary(
      for {
        a <- Arbitrary.arbitrary[A]
        f <- Arbitrary.arbitrary[Boolean].map(Function.const[Boolean, A])
      } yield Bivariant[A](f, a)
    )
  }

  case class Pred[A](run: A => Boolean)

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object ICons {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[ICons[A]] =
      Arbitrary(for {
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[IList[A]]
      } yield ICons(head, tail))
  }

  object IList {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[IList[A]] =
      Arbitrary(Arbitrary.arbitrary[Seq[A]].map(fromSeq))

    implicit def cogen[A: Cogen]: Cogen[IList[A]] =
      Cogen[Seq[A]].contramap(toList)

    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight[IList[T]](INil())(ICons.apply)

    def toList[T](list: IList[T]): List[T] = {
      @tailrec def loop(list: IList[T], acc: List[T]): List[T] = list match {
        case INil() => acc.reverse
        case ICons(head, tail) => loop(tail, head :: acc)
      }

      loop(list, Nil)
    }
  }

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Snoc[A]] =
      Arbitrary(Arbitrary.arbitrary[Seq[A]].map(fromSeq))

    def fromSeq[T](ts: Seq[T]): Snoc[T] =
      ts.foldLeft[Snoc[T]](SNil())(SCons.apply)

    def toList[T](snoc: Snoc[T]): List[T] = {
      @tailrec def loop(snoc: Snoc[T], acc: List[T]): List[T] = snoc match {
        case SNil() => acc
        case SCons(init, last) => loop(init, last :: acc)
      }

      loop(snoc, Nil)
    }
  }

  object SCons {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[SCons[A]] =
      Arbitrary(for {
        init <- Arbitrary.arbitrary[Snoc[A]]
        last <- Arbitrary.arbitrary[A]
      } yield SCons(init, last))
  }

  sealed trait Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Tree[A]] = {
      val leaf = Arbitrary.arbitrary[A].map(Leaf.apply)

      def tree(maxDepth: Int): Gen[Tree[A]] =
        if (maxDepth <= 0) leaf
        else Gen.oneOf(leaf, node(maxDepth))

      def node(maxDepth: Int): Gen[Tree[A]] = for {
        depthL <- Gen.choose(0, maxDepth - 1)
        depthR <- Gen.choose(0, maxDepth - 1)
        left <- tree(depthL)
        right <- tree(depthR)
      } yield Node(left, right)

      Arbitrary(Gen.sized(tree))
    }

    implicit def cogen[A: Cogen]: Cogen[Tree[A]] = {
      lazy val cogen: Cogen[Tree[A]] = Cogen { (seed, tree) =>
        tree match {
          case Leaf(value) => Cogen[A].perturb(seed, value)
          case Node(left, right) => cogen.perturb(cogen.perturb(seed, left), right)
        }
      }

      cogen
    }
  }

  final case class Foo(i: Int, b: Option[String])
  object Foo {

    implicit val cogen: Cogen[Foo] =
      Cogen[Int].contramap(_.i)

    implicit val arbitrary: Arbitrary[Foo] =
      Arbitrary(for {
        i <- Arbitrary.arbitrary[Int]
        b <- Arbitrary.arbitrary[Option[String]]
      } yield Foo(i, b))
  }

  final case class CommutativeFoo(i: Int, b: Option[Long])
  object CommutativeFoo {

    implicit val cogen: Cogen[CommutativeFoo] =
      Cogen[(Int, Option[Long])].contramap(x => (x.i, x.b))

    implicit val arbitrary: Arbitrary[CommutativeFoo] =
      Arbitrary(for {
        i <- Arbitrary.arbitrary[Int]
        b <- Arbitrary.arbitrary[Option[Long]]
      } yield CommutativeFoo(i, b))
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

  sealed trait GenericAdt[A]
  final case class GenericAdtCase[A](value: Option[A]) extends GenericAdt[A]

  object GenericAdt {

    implicit def arbitrary[A: Arbitrary]: Arbitrary[GenericAdt[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(GenericAdtCase.apply))

    implicit def cogen[A: Cogen]: Cogen[GenericAdt[A]] =
      Cogen[Option[A]].contramap({ case GenericAdtCase(value) => value })
  }

  final case class CaseClassWOption[A](value: Option[A])
  object CaseClassWOption {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[CaseClassWOption[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(apply))
  }

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

object TestEqInstances {
  import TestDefns._

  implicit def eqComplexProduct[T: Eq]: Eq[ComplexProduct[T]] =
    new Eq[ComplexProduct[T]] {
      val eqSet = Eq[Set[T]]
      val eqVec = Eq[Vector[T]]
      val eqOpt = Eq[Eval[Option[T]]]

      def eqv(x: ComplexProduct[T], y: ComplexProduct[T]) =
        x.lbl == y.lbl &&
          eqSet.eqv(x.set, y.set) &&
          eqVec.eqv(x.fns.map(_()), y.fns.map(_())) &&
          eqOpt.eqv(x.opt, y.opt)
    }

  implicit def eqBox[A: Eq]: Eq[Box[A]] =
    Eq.by(_.content)

  implicit val eqRecursive: Eq[Recursive] =
    Eq.fromUniversalEquals

  implicit def eqInterleaved[T: Eq]: Eq[Interleaved[T]] =
    Eq.by(i => (i.i, i.t, i.l, i.tt, i.s))

  implicit def eqIList[A](implicit A: Eq[A]): Eq[IList[A]] = new Eq[IList[A]] {
    @tailrec def eqv(x: IList[A], y: IList[A]): Boolean = (x, y) match {
      case (ICons(hx, tx), ICons(hy, ty)) => A.eqv(hx, hy) && eqv(tx, ty)
      case (INil(), INil()) => true
      case _ => false
    }
  }

  implicit def eqSnoc[A](implicit A: Eq[A]): Eq[Snoc[A]] = new Eq[Snoc[A]] {
    @tailrec def eqv(x: Snoc[A], y: Snoc[A]): Boolean = (x, y) match {
      case (SCons(ix, lx), SCons(iy, ly)) => A.eqv(lx, ly) && eqv(ix, iy)
      case (SNil(), SNil()) => true
      case _ => false
    }
  }

  implicit def eqICons[A: Eq]: Eq[ICons[A]] = Eq.by(identity[IList[A]])
  implicit def eqSCons[A: Eq]: Eq[SCons[A]] = Eq.by(identity[Snoc[A]])

  implicit def eqTree[A](implicit A: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match {
      case (Leaf(vx), Leaf(vy)) => A.eqv(vx, vy)
      case (Node(lx, rx), Node(ly, ry)) => eqv(lx, ly) && eqv(rx, ry)
      case _ => false
    }
  }

  implicit def eqBivariant[A](implicit A: Eq[A]): Eq[Bivariant[A]] = new Eq[Bivariant[A]] {
    override def eqv(x: Bivariant[A], y: Bivariant[A]): Boolean = (x, y) match {
      case (Bivariant(runX, storeX), Bivariant(runY, storeY)) =>
        A.eqv(storeX, storeY) && Eq[Boolean].eqv(runX(storeX), runY(storeY))
    }
  }

  implicit val eqFoo: Eq[Foo] =
    Eq.fromUniversalEquals

  implicit val eqCommutativeFoo: Eq[CommutativeFoo] =
    Eq.fromUniversalEquals

  implicit def eqGenericAdt[A: Eq]: Eq[GenericAdt[A]] = {
    val eqvOpt = Eq[Option[A]]
    Eq.instance { case (GenericAdtCase(vx), GenericAdtCase(vy)) => eqvOpt.eqv(vx, vy) }
  }

  implicit def eqCaseClassWOption[A: Eq]: Eq[CaseClassWOption[A]] =
    Eq.by(_.value)
}
