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
import scala.compiletime.*

class FunctorSuite extends KittensSuite:
  import TestDefns.*

  given ExhaustiveCheck[Predicate[Boolean]] =
    ExhaustiveCheck.instance(List(_ => true, _ => false, identity, !_))

  inline def functorTests[F[_]]: FunctorTests[F] =
    FunctorTests[F](summonInline)

  inline def testFunctor(inline context: String): Unit =
    checkAll(s"$context.Functor[IList]", functorTests[IList].functor[Int, String, Long])
    checkAll(s"$context.Functor[Tree]", functorTests[Tree].functor[Int, String, Long])
    checkAll(s"$context.Functor[GenericAdt]", functorTests[GenericAdt].functor[Int, String, Long])
    checkAll(s"$context.Functor[OptList]", functorTests[OptList].functor[Int, String, Long])
    checkAll(s"$context.Functor[ListSnoc]", functorTests[ListSnoc].functor[Int, String, Long])
    // FIXME: Testing `functorTests[AndChar].functor[Int, String, Long]` causes a ClassCastException
    checkAll(s"$context.Functor[AndChar]", functorTests[AndChar].functor[Int, String, Int])
    checkAll(s"$context.Functor[Interleaved]", functorTests[Interleaved].functor[Int, String, Long])
    checkAll(s"$context.Functor[NestedPred]", functorTests[NestedPred].functor[Boolean, Int, Boolean])
    checkAll(s"$context.Functor is Serializable", SerializableTests.serializable(summonInline[Functor[Tree]]))

  locally {
    import auto.functor.given
    testFunctor("auto")
  }

  locally {
    import semiInstances.given
    testFunctor("semiauto")
  }

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type Predicate[A] = A => Boolean
  type NestedPred[A] = Predicate[Predicate[A]]

  object semiInstances:
    given Functor[IList] = semiauto.functor
    given Functor[Tree] = semiauto.functor
    given Functor[GenericAdt] = semiauto.functor
    given Functor[OptList] = semiauto.functor
    given Functor[ListSnoc] = semiauto.functor
    given Functor[AndChar] = semiauto.functor
    given Functor[Interleaved] = semiauto.functor
    given Functor[NestedPred] = semiauto.functor

end FunctorSuite
