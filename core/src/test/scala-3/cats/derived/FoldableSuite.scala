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

import cats.laws.discipline.{FoldableTests, SerializableTests}
import cats.syntax.all.*
import org.scalacheck.{Arbitrary, Gen}
import scala.compiletime.*

class FoldableSuite extends KittensSuite:
  import FoldableSuite.*
  import TestDefns.*

  inline def tests[F[_]]: FoldableTests[F] =
    FoldableTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[IList]", tests[IList].foldable[Int, Long])
    checkAll(s"$instance[Tree]", tests[Tree].foldable[Int, Long])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].foldable[Int, Long])
    checkAll(s"$instance[OptList]", tests[OptList].foldable[Int, Long])
    checkAll(s"$instance[ListSnoc]", tests[ListSnoc].foldable[Int, Long])
    checkAll(s"$instance[AndChar]", tests[AndChar].foldable[Int, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].foldable[Int, Long])
    checkAll(s"$instance[BoxNel]", tests[BoxNel].foldable[Int, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].foldable[Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Foldable[Tree]]))

  locally {
    import auto.foldable.given
    validate("auto.foldable")
  }

  locally {
    import semiFoldable.given
    validate("semiauto.foldable")
  }

  locally {
    import derivedFoldable.*
    import derivedFoldable.given
    val instance = "derived.foldable"
    checkAll(s"$instance[IList]", tests[IList].foldable[Int, Long])
    checkAll(s"$instance[Tree]", tests[Tree].foldable[Int, Long])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].foldable[Int, Long])
    checkAll(s"$instance[AndChar]", tests[AndChar].foldable[Int, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].foldable[Int, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].foldable[Int, Long])
    checkAll(s"$instance[Single]", tests[Single].foldable[Int, Long])
    checkAll(s"$instance[Many]", tests[Many].foldable[Int, Long])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].foldable[Int, Long])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].foldable[Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Foldable[Tree]]))
  }

end FoldableSuite

object FoldableSuite:
  import TestDefns.*

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type BoxNel[A] = Box[Nel[A]]

  object semiFoldable:
    given Foldable[IList] = semiauto.foldable
    given Foldable[Tree] = semiauto.foldable
    given Foldable[GenericAdt] = semiauto.foldable
    given Foldable[OptList] = semiauto.foldable
    given Foldable[ListSnoc] = semiauto.foldable
    given Foldable[AndChar] = semiauto.foldable
    given Foldable[Interleaved] = semiauto.foldable
    given Foldable[BoxNel] = semiauto.foldable
    given Foldable[EnumK1] = semiauto.foldable

  object derivedFoldable:
    case class IList[A](x: TestDefns.IList[A]) derives Foldable
    case class Tree[A](x: TestDefns.Tree[A]) derives Foldable
    case class GenericAdt[A](x: TestDefns.GenericAdt[A]) derives Foldable
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Foldable
    case class EnumK1[A](x: TestDefns.EnumK1[A]) derives Foldable
    case class AndChar[A](x: FoldableSuite.AndChar[A]) derives Foldable
    case class Single[A](value: A) derives Foldable

    enum Many[+A] derives Foldable, Eq:
      case Naught
      case More(value: A, rest: Many[A])

    enum AtMostOne[+A] derives Foldable, Eq:
      case Naught
      case Single(value: A)

    enum AtLeastOne[+A] derives Foldable, Eq:
      case Single(value: A)
      case More(value: A, rest: Option[AtLeastOne[A]])

    given [A: Arbitrary]: Arbitrary[Many[A]] = Arbitrary(
      Gen.oneOf(
        Gen.const(Many.Naught),
        Gen.lzy(Arbitrary.arbitrary[(A, Many[A])].map(Many.More.apply))
      )
    )

    given [A: Arbitrary]: Arbitrary[AtMostOne[A]] = Arbitrary(
      Gen.oneOf(
        Gen.const(AtMostOne.Naught),
        Arbitrary.arbitrary[A].map(AtMostOne.Single.apply)
      )
    )

    given [A: Arbitrary]: Arbitrary[AtLeastOne[A]] = Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[A].map(AtLeastOne.Single.apply),
        Gen.lzy(Arbitrary.arbitrary[(A, Option[AtLeastOne[A]])].map(AtLeastOne.More.apply))
      )
    )

  final case class Nel[+A](head: A, tail: List[A])
  object Nel:
    given [A: Eq]: Eq[Nel[A]] =
      (x, y) => x.head === y.head && x.tail === y.tail

    given [A: Arbitrary]: Arbitrary[Nel[A]] =
      Arbitrary(for
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[List[A]]
      yield Nel(head, tail))

    given Foldable[Nel] with
      def foldLeft[A, B](fa: Nel[A], b: B)(f: (B, A) => B) = fa.tail.foldl(b)(f)
      def foldRight[A, B](fa: Nel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.tail.foldr(lb)(f)

end FoldableSuite
