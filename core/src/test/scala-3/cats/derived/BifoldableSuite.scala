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

import cats.laws.discipline.*
import cats.{Bifoldable, Foldable}
import shapeless3.deriving.K2.*

import scala.compiletime.*

class BifoldableSuite extends KittensSuite:
  import ADTs.*
  import BifoldableSuite.*

  inline def tests[F[_, _]]: BifoldableTests[F] =
    BifoldableTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Result]", tests[Result].bifoldable[String, Long, Int])
    checkAll(s"$instance[Nested]", tests[Nested].bifoldable[String, Long, Int])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bifoldable[Result]]))

  locally:
    import auto.bifoldable.given
    validate("auto.bifoldable")

  locally:
    import semiInstances.given
    validate("semiauto.bifoldable")

  locally:
    import strictInstances.given
    validate("strict.semiauto.bifoldable")

  locally:
    import derivedInstances.*
    val instance = "derived.bifunctor"
    checkAll(s"$instance[Result]", tests[Result].bifoldable[String, Long, Int])
    checkAll(s"$instance[Nested]", tests[Nested].bifoldable[String, Long, Int])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bifoldable[Result]]))

end BifoldableSuite

object BifoldableSuite:
  import ADTs.*

  type Nested[A, B] = Either[Option[A], Either[A, B]]

  object semiInstances:
    given Bifoldable[Result] = semiauto.bifoldable
    given Bifoldable[Nested] = semiauto.bifoldable

  object strictInstances:
    given [T]: Bifoldable[Const[T]] = semiauto.bifoldable
    given Bifoldable[Id1] = semiauto.bifoldable
    given Bifoldable[Id2] = semiauto.bifoldable
    given [F[_]: Foldable]: Bifoldable[Left1[F]] = semiauto.bifoldable
    given [F[_]: Foldable]: Bifoldable[Right1[F]] = semiauto.bifoldable
    given Bifoldable[Result] = strict.semiauto.bifoldable
    given Bifoldable[Nested] = strict.semiauto.bifoldable

  object derivedInstances:
    case class Result[A, E](x: ADTs.Result[A, E]) derives Bifoldable
    case class Nested[A, B](x: BifunctorSuite.Nested[A, B]) derives Bifoldable

end BifoldableSuite
