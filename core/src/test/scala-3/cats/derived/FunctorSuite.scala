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

import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import org.scalacheck.{Arbitrary, Gen}
import scala.compiletime.*

class FunctorSuite extends KittensSuite:
  import FunctorSuite.*
  import TestDefns.*

  given ExhaustiveCheck[Predicate[Boolean]] =
    ExhaustiveCheck.instance(List(_ => true, _ => false, identity, !_))

  inline def tests[F[_]]: FunctorTests[F] =
    FunctorTests[F](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[IList]", tests[IList].functor[Int, String, Long])
    checkAll(s"$instance[Tree]", tests[Tree].functor[Int, String, Long])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].functor[Int, String, Long])
    checkAll(s"$instance[OptList]", tests[OptList].functor[Int, String, Long])
    checkAll(s"$instance[ListSnoc]", tests[ListSnoc].functor[Int, String, Long])
    checkAll(s"$instance[AndChar]", tests[AndChar].functor[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].functor[Int, String, Long])
    checkAll(s"$instance[NestedPred]", tests[NestedPred].functor[Boolean, Int, Boolean])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].functor[Boolean, Int, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Functor[Tree]]))

  locally {
    import auto.functor.given
    validate("auto.functor")
  }

  locally {
    import semiFunctor.given
    validate("semiauto.functor")
  }

  locally {
    import derivedFunctor.*
    import derivedFunctor.given
    val instance = "derived.functor"
    checkAll(s"$instance[IList]", tests[IList].functor[Int, String, Long])
    checkAll(s"$instance[Tree]", tests[Tree].functor[Int, String, Long])
    checkAll(s"$instance[GenericAdt]", tests[GenericAdt].functor[Int, String, Long])
    checkAll(s"$instance[AndChar]", tests[AndChar].functor[Int, String, Long])
    checkAll(s"$instance[Interleaved]", tests[Interleaved].functor[Int, String, Long])
    checkAll(s"$instance[EnumK1]", tests[EnumK1].functor[Boolean, Int, Boolean])
    checkAll(s"$instance[Single]", tests[Single].functor[Boolean, Int, Boolean])
    checkAll(s"$instance[Many]", tests[Many].functor[Boolean, Int, Boolean])
    checkAll(s"$instance[AtMostOne]", tests[AtMostOne].functor[Boolean, Int, Boolean])
    checkAll(s"$instance[AtLeastOne]", tests[AtLeastOne].functor[Boolean, Int, Boolean])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Functor[Tree]]))
  }

end FunctorSuite

object FunctorSuite:
  import TestDefns.*

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type Predicate[A] = A => Boolean
  type NestedPred[A] = Predicate[Predicate[A]]

  object semiFunctor:
    given Functor[IList] = semiauto.functor
    given Functor[Tree] = semiauto.functor
    given Functor[GenericAdt] = semiauto.functor
    given Functor[OptList] = semiauto.functor
    given Functor[ListSnoc] = semiauto.functor
    given Functor[AndChar] = semiauto.functor
    given Functor[Interleaved] = semiauto.functor
    given Functor[NestedPred] = semiauto.functor
    given Functor[EnumK1] = semiauto.functor

  object derivedFunctor:
    case class IList[A](x: TestDefns.IList[A]) derives Functor
    case class Tree[A](x: TestDefns.Tree[A]) derives Functor
    case class GenericAdt[A](x: TestDefns.GenericAdt[A]) derives Functor
    case class Interleaved[A](x: TestDefns.Interleaved[A]) derives Functor
    case class EnumK1[A](x: TestDefns.EnumK1[A]) derives Functor
    case class AndChar[A](x: FoldableSuite.AndChar[A]) derives Functor
    case class Single[A](value: A) derives Functor

    enum Many[+A] derives Functor, Eq:
      case Naught
      case More(value: A, rest: Many[A])

    enum AtMostOne[+A] derives Functor, Eq:
      case Naught
      case Single(value: A)

    enum AtLeastOne[+A] derives Functor, Eq:
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

end FunctorSuite
