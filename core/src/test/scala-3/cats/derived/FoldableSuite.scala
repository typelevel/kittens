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

import cats.{Eval, Foldable}
import cats.laws.discipline.*
import cats.syntax.all.given
import scala.compiletime.*

class FoldableSuite extends KittensSuite:
  import FoldableSuite.*
  import ADTs.*

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
    checkAll(s"$instance[Many]", tests[Many].foldable[Int, Long])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].foldable[Int, Long])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].foldable[Int, Long])
    checkAll(s"$instance[Singletons]", tests[Singletons].foldable[Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Foldable[Tree]]))

  locally:
    import auto.foldable.given
    validate("auto.foldable")

  locally:
    import semiInstances.given
    validate("semiauto.foldable")

  locally:
    import derivedInstances.*
    val instance = "derived.foldable"
    checkAll(s"$instance[IList]", tests[IList].foldable[Int, Long])
    checkAll(s"$instance[Tree]", tests[Tree].foldable[Int, Long])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].foldable[Int, Long])
    checkAll(s"$instance[AndChar]", tests[AndChar].foldable[Int, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].foldable[Int, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].foldable[Int, Long])
    checkAll(s"$instance[Many]", tests[Many].foldable[Int, Long])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].foldable[Int, Long])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].foldable[Int, Long])
    checkAll(s"$instance[Singletons]", tests[Singletons].foldable[Int, Long])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Foldable[Tree]))

end FoldableSuite

object FoldableSuite:
  import ADTs.*

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type BoxNel[A] = Box[Nel[A]]

  object semiInstances:
    given Foldable[IList] = semiauto.foldable
    given Foldable[Tree] = semiauto.foldable
    given Foldable[GenericAdt] = semiauto.foldable
    given Foldable[OptList] = semiauto.foldable
    given Foldable[ListSnoc] = semiauto.foldable
    given Foldable[AndChar] = semiauto.foldable
    given Foldable[Interleaved] = semiauto.foldable
    given Foldable[BoxNel] = semiauto.foldable
    given Foldable[EnumK1] = semiauto.foldable
    given Foldable[Many] = semiauto.foldable
    given Foldable[AtMostOne] = semiauto.foldable
    given Foldable[AtLeastOne] = semiauto.foldable
    given Foldable[Singletons] = semiauto.foldable

  object derivedInstances:
    case class IList[A](x: ADTs.IList[A]) derives Foldable
    case class Tree[A](x: ADTs.Tree[A]) derives Foldable
    case class GenericAdt[A](x: ADTs.GenericAdt[A]) derives Foldable
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Foldable
    case class EnumK1[A](x: ADTs.EnumK1[A]) derives Foldable
    case class AndChar[A](x: FoldableSuite.AndChar[A]) derives Foldable
    case class Many[+A](x: ADTs.Many[A]) derives Foldable
    case class AtMostOne[+A](x: ADTs.AtMostOne[A]) derives Foldable
    case class AtLeastOne[+A](x: ADTs.AtLeastOne[A]) derives Foldable
    case class Singletons[A](x: ADTs.Singletons[A]) derives Foldable

  final case class Nel[+A](head: A, tail: List[A])
  object Nel:
    given Foldable[Nel] with
      def foldLeft[A, B](fa: Nel[A], b: B)(f: (B, A) => B) = fa.tail.foldl(b)(f)
      def foldRight[A, B](fa: Nel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.tail.foldr(lb)(f)

end FoldableSuite
