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
import cats.{Bifunctor, Functor}
import shapeless3.deriving.K2.*

import scala.compiletime.*

class BifunctorSuite extends KittensSuite:
  import ADTs.*
  import BifunctorSuite.*

  inline def tests[F[_, _]]: BifunctorTests[F] =
    BifunctorTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Result]", tests[Result].bifunctor[Int, Int, Int, String, String, String])
    checkAll(s"$instance[Nested]", tests[Nested].bifunctor[Int, Int, Int, String, String, String])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bifunctor[Result]]))

  locally:
    import auto.bifunctor.given
    validate("auto.bifunctor")

  locally:
    import semiInstances.given
    validate("semiauto.bifunctor")

  locally:
    import strictInstances.given
    validate("strict.semiauto.bifunctor")

  locally:
    import derivedInstances.*
    val instance = "derived.bifunctor"
    checkAll(s"$instance[Result]", tests[Result].bifunctor[Int, Int, Int, String, String, String])
    checkAll(s"$instance[Nested]", tests[Nested].bifunctor[Int, Int, Int, String, String, String])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bifunctor[Result]]))

end BifunctorSuite

object BifunctorSuite:
  import ADTs.*

  type Nested[A, B] = Either[Option[A], Either[A, B]]

  object semiInstances:
    given Bifunctor[Result] = semiauto.bifunctor
    given Bifunctor[Nested] = semiauto.bifunctor

  object strictInstances:
    given [T]: Bifunctor[Const[T]] = semiauto.bifunctor
    given Bifunctor[Id1] = semiauto.bifunctor
    given Bifunctor[Id2] = semiauto.bifunctor
    given [F[_]: Functor]: Bifunctor[Left1[F]] = semiauto.bifunctor
    given [F[_]: Functor]: Bifunctor[Right1[F]] = semiauto.bifunctor
    given Bifunctor[Result] = strict.semiauto.bifunctor
    given Bifunctor[Nested] = strict.semiauto.bifunctor

  object derivedInstances:
    case class Result[A, E](x: ADTs.Result[A, E]) derives Bifunctor
    case class Nested[A, B](x: BifunctorSuite.Nested[A, B]) derives Bifunctor

end BifunctorSuite
