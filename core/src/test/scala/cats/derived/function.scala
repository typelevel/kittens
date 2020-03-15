/*
 * Copyright (c) 2016 Miles Sabin
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

import cats.Eq
import cats.laws.discipline.{DistributiveTests, ExhaustiveCheck, MonadTests}
import cats.derived.function._
import cats.instances.all._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class FunctionSuite extends KittensSuite {
  type B = Boolean
  type F0[R] = () => R
  type F1[R] = B => R
  type F2[R] = (B, B) => R
  type F4[R] = (B, B, B, B) => R
  type F8[R] = (B, B, B, B, B, B, B, B) => R

  implicit val exhaustiveCheck4: ExhaustiveCheck[(B, B, B, B)] =
    ExhaustiveCheck.instance(for {
      (a, b) <- ExhaustiveCheck[(B, B)].allValues
      (c, d) <- ExhaustiveCheck[(B, B)].allValues
    } yield (a, b, c, d))

  implicit val exhaustiveCheck8: ExhaustiveCheck[(B, B, B, B, B, B, B, B)] =
    ExhaustiveCheck.instance(for {
      (a, b, c, d) <- ExhaustiveCheck[(B, B, B, B)].allValues
      (e, f, g, h) <- ExhaustiveCheck[(B, B, B, B)].allValues
    } yield (a, b, c, d, e, f, g, h))

  implicit def eqFn4[R: Eq]: Eq[F4[R]] = Eq.by(_.tupled)
  implicit def eqFn8[R: Eq]: Eq[F8[R]] = Eq.by(_.tupled)

  checkAll("Monad[Function0]", MonadTests[F0].monad[B, B, B])
  checkAll("Monad[Function1]", MonadTests[F1].monad[B, B, B])
  checkAll("Monad[Function2]", MonadTests[F2].monad[B, B, B])
  checkAll("Monad[Function4]", MonadTests[F4].monad[B, B, B])
  checkAll("Monad[Function8]", MonadTests[F8].monad[B, B, B])

  checkAll("Distributive[Function0]", DistributiveTests[F0].distributive[B, B, B, Option, Function0])
  checkAll("Distributive[Function1]", DistributiveTests[F1].distributive[B, B, B, Option, Function0])
  checkAll("Distributive[Function2]", DistributiveTests[F2].distributive[B, B, B, Option, Function0])
  checkAll("Distributive[Function4]", DistributiveTests[F4].distributive[B, B, B, Option, Function0])
  checkAll("Distributive[Function8]", DistributiveTests[F8].distributive[B, B, B, Option, Function0])
}
