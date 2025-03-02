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
import cats.{Bifoldable, Bitraverse, Traverse}

import scala.compiletime.*

class BitraverseSuite extends KittensSuite:
  import ADTs.*
  import BitraverseSuite.*

  inline def tests[F[_, _]]: BitraverseTests[F] =
    BitraverseTests[F](using summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[Result]", tests[Result].bitraverse[Option, Int, Int, Int, Int, Int, Int])
    checkAll(s"$instance[Nested]", tests[Nested].bitraverse[Option, Int, Int, Int, Int, Int, Int])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bitraverse[Result]]))

  locally:
    import auto.bitraverse.given
    validate("auto.bitraverse")

  locally:
    import semiInstances.given
    validate("semiauto.bitraverse")

  locally:
    import strictInstances.given
    validate("strict.semiauto.bitraverse")

  locally:
    import derivedInstances.*
    val instance = "derived.bitraverse"
    checkAll(s"$instance[Result]", tests[Result].bitraverse[Option, Int, Int, Int, Int, Int, Int])
    checkAll(s"$instance[Nested]", tests[Nested].bitraverse[Option, Int, Int, Int, Int, Int, Int])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Bitraverse[Result]]))

end BitraverseSuite

object BitraverseSuite:
  import ADTs.*

  type Nested[A, B] = Either[Option[A], Either[A, B]]

  object semiInstances:
    given Bitraverse[Result] = semiauto.bitraverse
    given Bitraverse[Nested] = semiauto.bitraverse

  object strictInstances:
    given [T]: Bitraverse[[_, _] =>> T] = semiauto.bitraverse
    given leftId: Bitraverse[[a, _] =>> a] = semiauto.bitraverse
    given rightId: Bitraverse[[_, b] =>> b] = semiauto.bitraverse
    given left[F[_]: Traverse]: Bitraverse[[a, _] =>> F[a]] = semiauto.bitraverse
    given right[F[_]: Traverse]: Bitraverse[[_, b] =>> F[b]] = semiauto.bitraverse
    given Bitraverse[Result] = strict.semiauto.bitraverse
    given Bitraverse[Nested] = strict.semiauto.bitraverse

  object derivedInstances:
    case class Result[A, E](x: ADTs.Result[A, E]) derives Bitraverse
    case class Nested[A, B](x: BifunctorSuite.Nested[A, B]) derives Bitraverse

end BitraverseSuite
