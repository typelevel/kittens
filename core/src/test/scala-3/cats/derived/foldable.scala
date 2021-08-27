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
import org.scalacheck.Arbitrary
import scala.compiletime.*

class FoldableSuite extends KittensSuite:
  import FoldableSuite.*
  import TestDefns.*

  inline def foldableTests[F[_]]: FoldableTests[F] =
    FoldableTests[F](summonInline)

  inline def testFoldable(inline context: String): Unit =
    checkAll(s"$context.Foldable[IList]", foldableTests[IList].foldable[Int, Long])
    checkAll(s"$context.Foldable[Tree]", foldableTests[Tree].foldable[Int, Long])
    checkAll(s"$context.Foldable[GenericAdt]", foldableTests[GenericAdt].foldable[Int, Long])
    checkAll(s"$context.Foldable[OptList]", foldableTests[OptList].foldable[Int, Long])
    checkAll(s"$context.Foldable[ListSnoc]", foldableTests[ListSnoc].foldable[Int, Long])
    checkAll(s"$context.Foldable[AndChar]", foldableTests[AndChar].foldable[Int, Long])
    checkAll(s"$context.Foldable[Interleaved]", foldableTests[Interleaved].foldable[Int, Long])
    checkAll(s"$context.Foldable[BoxNel]", foldableTests[BoxNel].foldable[Int, Long])
    checkAll(s"$context.Foldable is Serializable", SerializableTests.serializable(summonInline[Foldable[Tree]]))

  locally {
    import auto.foldable.given
    testFoldable("auto")
  }

  locally {
    import semiInstances.given
    testFoldable("semiauto")
  }

end FoldableSuite

object FoldableSuite:
  import TestDefns.*

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
