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
import cats.syntax.all.given
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.annotation.tailrec

object TestDefns:

  enum EnumK0:
    case LeafS(value: String)
    case LeafI(value: Int)

  object EnumK0:
    given Arbitrary[EnumK0] = Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[String].map(LeafS.apply),
        Arbitrary.arbitrary[Int].map(LeafI.apply)
      )
    )

    given Cogen[EnumK0] = Cogen[Either[String, Int]].contramap {
      case LeafS(s) => Left(s)
      case LeafI(i) => Right(i)
    }

  enum EnumK1[A]:
    case Leaf(value: A)

  object EnumK1:
    given [A](using Arbitrary[A]): Arbitrary[EnumK1[A]] = Arbitrary(Arbitrary.arbitrary[A].map(Leaf.apply))

  enum EnumK1Contra[-A]:
    case Leaf(value: A => Unit)

  object EnumK1Contra:
    given [A](using Arbitrary[A => Unit]): Arbitrary[EnumK1Contra[A]] = Arbitrary(
      Arbitrary.arbitrary[A => Unit].map(Leaf.apply)
    )

  enum EnumK1Inv[A]:
    case Leaf(cov: A, contra: A => Unit)

  object EnumK1Inv:
    given [A](using Arbitrary[A], Arbitrary[A => Unit]): Arbitrary[EnumK1Inv[A]] = Arbitrary(
      for
        cov <- Arbitrary.arbitrary[A]
        contra <- Arbitrary.arbitrary[A => Unit]
      yield Leaf(cov, contra)
    )

  sealed trait Rgb
  object Rgb:
    case object Red extends Rgb
    case object Green extends Rgb
    case object Blue extends Rgb

  final case class ComplexProduct[T](lbl: String, set: Set[T], fns: Vector[() => T], opt: Eval[Option[T]])
  object ComplexProduct:
    given [T: Arbitrary]: Arbitrary[ComplexProduct[T]] = Arbitrary(for
      lbl <- Arbitrary.arbitrary[String]
      set <- Arbitrary.arbitrary[Set[T]]
      vec <- Arbitrary.arbitrary[Vector[T]]
      fns = vec.map(x => () => x)
      opt <- Arbitrary.arbitrary[Option[T]]
    yield ComplexProduct(lbl, set, fns, Eval.now(opt)))

  final case class Box[+A](content: A)
  object Box:
    given [A: Arbitrary]: Arbitrary[Box[A]] = Arbitrary(Arbitrary.arbitrary[A].map(apply))
    given [A: Cogen]: Cogen[Box[A]] = Cogen[A].contramap(_.content)

  final case class Recursive(i: Int, is: Option[Recursive])
  object Recursive extends ((Int, Option[Recursive]) => Recursive):
    given Arbitrary[Recursive] =
      def recursive(size: Int): Gen[Recursive] = for
        i <- Arbitrary.arbitrary[Int]
        is <- if (size <= 0) Gen.const(None) else Gen.option(recursive(size / 2))
      yield Recursive(i, is)
      Arbitrary(Gen.sized(recursive))

    given Cogen[Recursive] =
      @tailrec def perturb(seed: Seed, rec: Recursive): Seed =
        val i = Cogen[Int].perturb(seed, rec.i)
        rec.is match
          case Some(is) => perturb(i, is)
          case None => i
      Cogen(perturb _)

  final case class Interleaved[T](i: Int, t: T, l: Long, tt: Vector[T], s: String)
  object Interleaved:
    def empty[T](t: T): Interleaved[T] =
      Interleaved(0, t, 0, Vector.empty, "")

    given [T: Arbitrary]: Arbitrary[Interleaved[T]] =
      Arbitrary(Arbitrary.arbitrary[(Int, T, Long, Vector[T], String)].map((apply[T] _).tupled))

    given [T: Cogen]: Cogen[Interleaved[T]] =
      Cogen[(Int, T, Long, Vector[T], String)].contramap(x => (x.i, x.t, x.l, x.tt, x.s))

  case class Bivariant[A](run: A => Boolean, store: A)
  object Bivariant:
    given [A: Arbitrary]: Arbitrary[Bivariant[A]] = Arbitrary(
      for
        a <- Arbitrary.arbitrary[A]
        f <- Arbitrary.arbitrary[Boolean].map(Function.const[Boolean, A])
      yield Bivariant[A](f, a)
    )

  case class Pred[A](run: A => Boolean)

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object ICons:

    given [A: Arbitrary]: Arbitrary[ICons[A]] =
      Arbitrary(for
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[IList[A]]
      yield ICons(head, tail))

  object IList:

    given [A: Arbitrary]: Arbitrary[IList[A]] =
      Arbitrary(Arbitrary.arbitrary[Seq[A]].map(fromSeq))

    given [A: Cogen]: Cogen[IList[A]] =
      Cogen[Seq[A]].contramap(toList)

    def fromSeq[T](ts: Seq[T]): IList[T] =
      ts.foldRight[IList[T]](INil())(ICons.apply)

    def toList[T](list: IList[T]): List[T] =
      @tailrec def loop(list: IList[T], acc: List[T]): List[T] = list match
        case INil() => acc.reverse
        case ICons(head, tail) => loop(tail, head :: acc)
      loop(list, Nil)

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc:

    given [A: Arbitrary]: Arbitrary[Snoc[A]] =
      Arbitrary(Arbitrary.arbitrary[Seq[A]].map(fromSeq))

    def fromSeq[T](ts: Seq[T]): Snoc[T] =
      ts.foldLeft[Snoc[T]](SNil())(SCons.apply)

    def toList[T](snoc: Snoc[T]): List[T] =
      @tailrec def loop(snoc: Snoc[T], acc: List[T]): List[T] = snoc match
        case SNil() => acc
        case SCons(init, last) => loop(init, last :: acc)
      loop(snoc, Nil)

  object SCons:

    given [A: Arbitrary]: Arbitrary[SCons[A]] =
      Arbitrary(for
        init <- Arbitrary.arbitrary[Snoc[A]]
        last <- Arbitrary.arbitrary[A]
      yield SCons(init, last))

  sealed trait Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree:
    given [A: Arbitrary]: Arbitrary[Tree[A]] =
      val leaf = Arbitrary.arbitrary[A].map(Leaf.apply)

      def tree(maxDepth: Int): Gen[Tree[A]] =
        if (maxDepth <= 0) leaf
        else Gen.oneOf(leaf, node(maxDepth))

      def node(maxDepth: Int): Gen[Tree[A]] = for
        depthL <- Gen.choose(0, maxDepth - 1)
        depthR <- Gen.choose(0, maxDepth - 1)
        left <- tree(depthL)
        right <- tree(depthR)
      yield Node(left, right)

      Arbitrary(Gen.sized(tree))

    given [A: Cogen]: Cogen[Tree[A]] =
      lazy val cogen: Cogen[Tree[A]] = Cogen { (seed, tree) =>
        tree match
          case Leaf(value) => Cogen[A].perturb(seed, value)
          case Node(left, right) => cogen.perturb(cogen.perturb(seed, left), right)
      }

      cogen

  final case class Foo(i: Int, b: Option[String])
  object Foo:

    given Cogen[Foo] =
      Cogen[Int].contramap(_.i)

    given Arbitrary[Foo] =
      Arbitrary(for
        i <- Arbitrary.arbitrary[Int]
        b <- Arbitrary.arbitrary[Option[String]]
      yield Foo(i, b))

  final case class CommutativeFoo(i: Int, b: Option[Long])
  object CommutativeFoo:

    given Cogen[CommutativeFoo] =
      Cogen[(Int, Option[Long])].contramap(x => (x.i, x.b))

    given Arbitrary[CommutativeFoo] =
      Arbitrary(for
        i <- Arbitrary.arbitrary[Int]
        b <- Arbitrary.arbitrary[Option[Long]]
      yield CommutativeFoo(i, b))

  case class Inner(i: Int)
  case class Outer(in: Inner)

  object Inner:

    given Arbitrary[Inner] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    given Cogen[Inner] =
      Cogen[Int].contramap(_.i)

  object Outer:

    given Arbitrary[Outer] =
      Arbitrary(Arbitrary.arbitrary[Inner].map(apply))

    given Cogen[Outer] =
      Cogen[Inner].contramap(_.in)

  sealed trait IntTree
  final case class IntLeaf(t: Int) extends IntTree
  final case class IntNode(l: IntTree, r: IntTree) extends IntTree

  sealed trait GenericAdt[A]
  final case class GenericAdtCase[A](value: Option[A]) extends GenericAdt[A]

  object GenericAdt:

    given [A: Arbitrary]: Arbitrary[GenericAdt[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(GenericAdtCase.apply))

    given [A: Cogen]: Cogen[GenericAdt[A]] =
      Cogen[Option[A]].contramap { case GenericAdtCase(value) => value }

  final case class CaseClassWOption[A](value: Option[A])
  object CaseClassWOption:
    given [A: Arbitrary]: Arbitrary[CaseClassWOption[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(apply))

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

end TestDefns

trait TestEqInstances:
  import TestDefns.*

  given [T: Eq]: Eq[ComplexProduct[T]] with
    val eqSet = Eq[Set[T]]
    val eqVec = Eq[Vector[T]]
    val eqOpt = Eq[Eval[Option[T]]]

    def eqv(x: ComplexProduct[T], y: ComplexProduct[T]) =
      x.lbl == y.lbl &&
        eqSet.eqv(x.set, y.set) &&
        eqVec.eqv(x.fns.map(_()), y.fns.map(_())) &&
        eqOpt.eqv(x.opt, y.opt)

  given [A: Eq]: Eq[Box[A]] =
    Eq.by(_.content)

  given Eq[Recursive] =
    Eq.fromUniversalEquals

  given [T: Eq]: Eq[Interleaved[T]] =
    Eq.by(i => (i.i, i.t, i.l, i.tt, i.s))

  given [A](using A: Eq[A]): Eq[IList[A]] = new Eq[IList[A]]:
    @tailrec def eqv(x: IList[A], y: IList[A]): Boolean = (x, y) match
      case (ICons(hx, tx), ICons(hy, ty)) => A.eqv(hx, hy) && eqv(tx, ty)
      case (INil(), INil()) => true
      case _ => false

  given [A](using A: Eq[A]): Eq[Snoc[A]] = new Eq[Snoc[A]]:
    @tailrec def eqv(x: Snoc[A], y: Snoc[A]): Boolean = (x, y) match
      case (SCons(ix, lx), SCons(iy, ly)) => A.eqv(lx, ly) && eqv(ix, iy)
      case (SNil(), SNil()) => true
      case _ => false

  given [A: Eq]: Eq[ICons[A]] = Eq.by(identity[IList[A]])
  given [A: Eq]: Eq[SCons[A]] = Eq.by(identity[Snoc[A]])

  given [A](using A: Eq[A]): Eq[Tree[A]] with
    def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match
      case (Leaf(vx), Leaf(vy)) => A.eqv(vx, vy)
      case (Node(lx, rx), Node(ly, ry)) => eqv(lx, ly) && eqv(rx, ry)
      case _ => false

  given [A](using A: Eq[A]): Eq[Bivariant[A]] with
    def eqv(x: Bivariant[A], y: Bivariant[A]): Boolean = (x, y) match
      case (Bivariant(runX, storeX), Bivariant(runY, storeY)) =>
        A.eqv(storeX, storeY) && Eq[Boolean].eqv(runX(storeX), runY(storeY))

  given Eq[Foo] = Eq.fromUniversalEquals

  given Eq[CommutativeFoo] = Eq.fromUniversalEquals

  given [A: Eq]: Eq[GenericAdt[A]] =
    val eqvOpt = Eq[Option[A]]
    Eq.instance { case (GenericAdtCase(vx), GenericAdtCase(vy)) => eqvOpt.eqv(vx, vy) }

  given [A: Eq]: Eq[CaseClassWOption[A]] = Eq.by(_.value)

  given Eq[Inner] = Eq.fromUniversalEquals

  given Eq[Outer] = Eq.fromUniversalEquals

  given Eq[EnumK0] =
    import EnumK0.*
    Eq.instance {
      case (LeafS(s1), LeafS(s2)) => s1 === s2
      case (LeafI(i1), LeafI(i2)) => i1 === i2
      case _ => false
    }

  given [A](using Eq[A]): Eq[EnumK1[A]] =
    import EnumK1.*
    Eq.instance { case (Leaf(v1), Leaf(v2)) =>
      v1 === v2
    }

  given [A](using Eq[A => Unit]): Eq[EnumK1Contra[A]] =
    import EnumK1Contra.*
    Eq.instance { case (Leaf(v1), Leaf(v2)) =>
      v1 === v2
    }

  given [A](using Eq[A], Eq[A => Unit]): Eq[EnumK1Inv[A]] =
    import EnumK1Inv.*
    Eq.instance { case (Leaf(cov1, contra1), Leaf(cov2, contra2)) =>
      cov1 === cov2 && contra1 === contra2
    }

end TestEqInstances
