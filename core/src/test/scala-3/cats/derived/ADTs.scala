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

object ADTs:
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
    case Rec(l: EnumK1[A], r: EnumK1[A])

  object EnumK1:
    given [A: Arbitrary]: Arbitrary[EnumK1[A]] =
      Arbitrary(Gen.recursive { rec =>
        Gen.sized { size =>
          val leaf = Arbitrary.arbitrary[A].map(Leaf.apply)
          if size == 0 then leaf
          else Gen.resize(size / 2, Gen.oneOf(leaf, Gen.zip(rec, rec).map(Rec.apply)))
        }
      })

  enum EnumK1Contra[-A]:
    case Leaf(value: A => Unit)
    case Rec(l: EnumK1Contra[A], r: EnumK1Contra[A])

  object EnumK1Contra:
    given [A](using Arbitrary[A => Unit]): Arbitrary[EnumK1Contra[A]] =
      Arbitrary(Gen.recursive { rec =>
        Gen.sized { size =>
          val leaf = Arbitrary.arbitrary[A => Unit].map(Leaf.apply)
          if size == 0 then leaf
          else Gen.resize(size / 2, Gen.oneOf(leaf, Gen.zip(rec, rec).map(Rec.apply)))
        }
      })

  enum EnumK1Inv[A]:
    case Leaf(cov: A, contra: A => Unit)
    case Rec(l: EnumK1Inv[A], r: EnumK1Inv[A])

  object EnumK1Inv:
    given [A: Arbitrary](using Arbitrary[A => Unit]): Arbitrary[EnumK1Inv[A]] =
      Arbitrary(Gen.recursive { rec =>
        Gen.sized { size =>
          val leaf = for
            cov <- Arbitrary.arbitrary[A]
            contra <- Arbitrary.arbitrary[A => Unit]
          yield Leaf(cov, contra)
          if size == 0 then leaf
          else Gen.resize(size / 2, Gen.oneOf(leaf, Gen.zip(rec, rec).map(Rec.apply)))
        }
      })

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

  final case class Recursive(i: Int, is: Option[Recursive])
  object Recursive extends ((Int, Option[Recursive]) => Recursive):
    given Arbitrary[Recursive] =
      def recursive(size: Int): Gen[Recursive] = for
        i <- Arbitrary.arbitrary[Int]
        is <- if size <= 0 then Gen.const(None) else Gen.option(recursive(size / 2))
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

  case class Bivariant[A](run: A => Boolean, store: A)
  case class Pred[A](run: A => Boolean)

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object ICons:
    given [A: Arbitrary]: Arbitrary[ICons[A]] = Arbitrary(for
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
    given [A: Arbitrary]: Arbitrary[SCons[A]] = Arbitrary(for
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
        if maxDepth <= 0 then leaf
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
  final case class CommutativeFoo(i: Int, b: Option[Long])

  case class Inner(i: Int)
  case class Outer(in: Inner)

  sealed trait IntTree
  final case class IntLeaf(t: Int) extends IntTree
  final case class IntNode(l: IntTree, r: IntTree) extends IntTree

  object IntTree

  sealed trait GenericAdt[A]
  final case class GenericAdtCase[A](value: Option[A]) extends GenericAdt[A]

  object GenericAdt:
    given [A: Arbitrary]: Arbitrary[GenericAdt[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(GenericAdtCase.apply))
    given [A: Cogen]: Cogen[GenericAdt[A]] =
      Cogen[Option[A]].contramap { case GenericAdtCase(value) => value }

  final case class CaseClassWOption[A](value: Option[A])

  final case class First(value: String)
  final case class Second(value: String)
  final case class Middle(first: First, second: Option[Second])
  final case class Top(middle: Middle)

  final case class Address(street: String, city: String, state: String)
  final case class ContactInfo(phoneNumber: String, address: Address)
  final case class People(name: String, contactInfo: ContactInfo)

  final case class ListField(a: String, b: List[ListFieldChild])
  final case class ListFieldChild(c: Int)

  final case class Singletons[A](value: A, str: "Scala" = "Scala", lng: 42L = 42L, dbl: 3.14 = 3.14)

  enum Many[+A] derives Eq:
    case Naught
    case More(value: A, rest: Many[A])

  enum AtMostOne[+A] derives Eq:
    case Naught
    case Single(value: A)

  enum AtLeastOne[+A] derives Eq:
    case Single(value: A)
    case More(value: A, rest: Option[AtLeastOne[A]])

  object Many:
    given [A: Arbitrary]: Arbitrary[Many[A]] = Arbitrary(
      Gen.oneOf(
        Gen.const(Many.Naught),
        Gen.lzy(Arbitrary.arbitrary[(A, Many[A])].map(Many.More.apply))
      )
    )

  object AtMostOne:
    given [A: Arbitrary]: Arbitrary[AtMostOne[A]] = Arbitrary(
      Gen.oneOf(
        Gen.const(AtMostOne.Naught),
        Arbitrary.arbitrary[A].map(AtMostOne.Single.apply)
      )
    )

  object AtLeastOne:
    given [A: Arbitrary]: Arbitrary[AtLeastOne[A]] = Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[A].map(AtLeastOne.Single.apply),
        Gen.lzy(Arbitrary.arbitrary[(A, Option[AtLeastOne[A]])].map(AtLeastOne.More.apply))
      )
    )

  trait EqInstances:
    import ADTs.*

    given Eq[Recursive] = Eq.fromUniversalEquals
    given [A: Eq]: Eq[ICons[A]] = Eq.by(identity[IList[A]])
    given [A: Eq]: Eq[SCons[A]] = Eq.by(identity[Snoc[A]])
    given [A: Eq]: Eq[GenericAdt[A]] = Eq.by { case GenericAdtCase(v) => v }

    given [A: Eq]: Eq[IList[A]] with
      @tailrec final def eqv(x: IList[A], y: IList[A]): Boolean = (x, y) match
        case (ICons(hx, tx), ICons(hy, ty)) => hx === hy && eqv(tx, ty)
        case (INil(), INil()) => true
        case _ => false

    given [A: Eq]: Eq[Snoc[A]] with
      @tailrec final def eqv(x: Snoc[A], y: Snoc[A]): Boolean = (x, y) match
        case (SCons(ix, lx), SCons(iy, ly)) => lx === ly && eqv(ix, iy)
        case (SNil(), SNil()) => true
        case _ => false

    given [A: Eq]: Eq[Tree[A]] with
      def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match
        case (Leaf(vx), Leaf(vy)) => vx === vy
        case (Node(lx, rx), Node(ly, ry)) => eqv(lx, ly) && eqv(rx, ry)
        case _ => false

    given [A: Eq]: Eq[Bivariant[A]] =
      case (Bivariant(runX, storeX), Bivariant(runY, storeY)) =>
        storeX === storeY && runX(storeX) == runY(storeY)

    given Eq[EnumK0] =
      case (EnumK0.LeafS(s1), EnumK0.LeafS(s2)) => s1 === s2
      case (EnumK0.LeafI(i1), EnumK0.LeafI(i2)) => i1 === i2
      case _ => false

    given [A: Eq]: Eq[EnumK1[A]] =
      case (EnumK1.Leaf(v1), EnumK1.Leaf(v2)) => v1 === v2
      case (EnumK1.Rec(l1, r1), EnumK1.Rec(l2, r2)) => l1 === l2 && r1 === r2
      case _ => false

    given [A](using Eq[A => Unit]): Eq[EnumK1Contra[A]] =
      case (EnumK1Contra.Leaf(v1), EnumK1Contra.Leaf(v2)) => v1 === v2
      case (EnumK1Contra.Rec(l1, r1), EnumK1Contra.Rec(l2, r2)) => l1 === l2 && r1 === r2
      case _ => false

    given [A: Eq](using Eq[A => Unit]): Eq[EnumK1Inv[A]] =
      case (EnumK1Inv.Leaf(cov1, contra1), EnumK1Inv.Leaf(cov2, contra2)) => cov1 === cov2 && contra1 === contra2
      case (EnumK1Inv.Rec(l1, r1), EnumK1Inv.Rec(l2, r2)) => l1 === l2 && r1 === r2
      case _ => false

  end EqInstances
end ADTs
